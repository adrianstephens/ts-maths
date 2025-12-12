import { applyRulesEgraph } from '../dist/egraph';
import { generalRules, toMVPoly, fromMVPoly } from '../dist/symbolicRules';
import { Polynomial } from '../dist/polynomial';
import { symbolic, symbolicOperators } from '../dist/symbolic';
import { parse } from '../dist/string';
import complex from '../dist/complex';
import { groebnerBasis, lexOrder } from '../dist/mvpolynomial';

function groebnerCheck(outExpr: symbolic, Uexpr: symbolic, Vexpr: symbolic) {
	const alpha = symbolic.variable('__alpha_g');
	const sVar = symbolic.variable('__s_g');

	const variables = [alpha, sVar];
	const p1 = alpha.npow(3).sub(Uexpr); // alpha^3 - U
	const p2 = sVar.sub(alpha).npow(3).sub(Vexpr); // (s - alpha)^3 - V
	const f1 = toMVPoly(p1, variables); // alpha^3 - U
	const f2 = toMVPoly(p2, variables); // (s - alpha)^3 - V

	const G = groebnerBasis([f1, f2], lexOrder);
	// find an eliminant polynomial that does not contain alpha
	for (const g of G) {
		if (!g.contains(0)) {
			const sub = fromMVPoly(g, [alpha, outExpr]);
			if (sub === symbolic.zero)
				return true;
		}
	}

	return false;
}

// Minimal reproducer: build the degree-3 polynomial used in tests and simplify its real roots
async function run() {

	const eqn = parse(symbolicOperators, `
1 / (3 * 2^(1/3)) * (
    (
        2((A + B + C)^3)
        + 27(A * B * C)
        - 9(((A + B) * C + A * B) * (A + B + C))
        + 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^0.5)
    )^(1/3)
)
+ 1 / (3 * 2^(1/3)) * (
    (
        -2((A + B + C)^3)
        - 27(A * B * C)
        + 9(((A + B) * C + A * B) * (A + B + C))
        + 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^0.5)
    )^(1/3)
)
+ A / 3
+ B / 3
+ C / 3
`);

	const a = parse(symbolicOperators, `
2((A + B + C)^3)
+ 27(A * B * C)
- 9(((A + B) * C + A * B) * (A + B + C))
+ 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^(1/2))
`);

	const b = parse(symbolicOperators, `
-2((A + B + C)^3)
- 27(A * B * C)
+ 9(((A + B) * C + A * B) * (A + B + C))
+ 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^(1/2))
`);

	// Deterministic Groebner-based check
	const out = a.npow(1 / 3).add(b.npow(1 / 3)).npow(3);
	groebnerCheck(out, a, b);

	const rule = generalRules.find(i => i.name === 'cbrt-sum-cubic-identity-contract');
	const bindings = rule?.match(eqn, { exact: false });
	console.log('direct rule match bindings:', bindings ? Object.keys(bindings) : null);
	return;

	let poly = Polynomial<symbolic>([symbolic.from(1)]);
	for (let j = 1; j < 4; ++j) {
		poly = poly.mul(Polynomial<symbolic>([symbolic.variable('ABCDEFGH'[j - 1]).neg(), symbolic.from(1)]));
		if (j !== 3) continue; // only run the degree-3 case (this is where heavy radicals appear)

		console.log(`degree ${j} polynomial: ${poly.toString()}`);
		const roots = poly.realRoots();
		console.log('roots:');
		console.log(roots.map(String).join('\n'));
		console.log('\nstart simplifying roots (instrumentation on MATHS_EXPAND_INSTRUMENT=1)');

		const bindings = {
			A: complex(1),
			B: complex(2),
			C: complex(3),
			D: complex(4),
			E: complex(5),
			F: complex(6),
			G: complex(7),
			H: complex(8),
		};

		function validate(a: symbolic, b: symbolic) {
			const va = a.evaluateT(complex, bindings);
			const vb = b.evaluateT(complex, bindings);
			if (va && vb && (va.approx(vb, 1e-10) || va.approx(vb.neg(), 1e-10)))
				return true;
			return false;
		}

		const maxExpansions = Number(process.env.MATHS_EGRAPH_MAX_EXPANSIONS ?? '16');
		const maxRounds = Number(process.env.MATHS_EGRAPH_MAX_ROUNDS ?? '6');
		console.log(`egraph: maxExpansions=${maxExpansions}, maxRounds=${maxRounds}`);

		for (const r of roots) {
			const timeoutMs = 20000;
			const start = Date.now();
			const callback = () => {
				if (Date.now() - start > timeoutMs) {
					console.warn(`egraph-diagnostic: timeout of ${timeoutMs}ms`);
					return false;
				}
				return true;
			};

			console.log('\n--- simplifying root ---');
			console.log(String(r));
			console.log(r);
			const r2 = applyRulesEgraph(r, generalRules, { verbose: true, debugNode: 'all', callback, validate, maxExpansions, maxRounds });
			console.log('\n=> simplified:');
			console.log(String(r2));
		}
	}
}

run().catch(err => { console.error(err); process.exitCode = 1; });
