import { applyRulesEgraph } from '../dist/egraph';
import { generalRules } from '../dist/symbolicRules';
import { Polynomial } from '../dist/polynomial';
import { symbolic, symbolicOperators } from '../dist/symbolic';
import { parse } from '../dist/string';
import complex, { complexOps } from '../dist/complex';

// Minimal reproducer: build the degree-3 polynomial used in tests and simplify its real roots
async function run() {

  const eqn = parse(symbolicOperators, `
0.26456684199469993(
    (
        2((A + B + C)^3)
        + 27(A * B * C)
        - 9(((A + B) * C + A * B) * (A + B + C))
        + 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^0.5)
    )^0.3333333333333333
)
+ 0.26456684199469993(
    (
        -2((A + B + C)^3)
        - 27(A * B * C)
        + 9(((A + B) * C + A * B) * (A + B + C))
        + 54((((3((A + B) * C) - (A + B + C)^2 + 3(A * B))^3) / 729 + ((-2((A + B + C)^3) - 27(A * B * C) + 9(((A + B) * C + A * B) * (A + B + C)))^2) / 2916)^0.5)
    )^0.3333333333333333
)
+ A / 3
+ B / 3
+ C / 3
`);
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
      const va = a.evaluateT(complexOps, bindings);
      const vb = b.evaluateT(complexOps, bindings);
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
