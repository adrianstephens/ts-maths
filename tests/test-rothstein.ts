import { Polynomial } from '../src/polynomial';
import { rothsteinPartial, rothsteinResidues, Residue } from '../src/factors';

function assert(cond: boolean, msg: string): void {
	if (!cond) throw new Error(msg);
}

function checkResiduesInvariant(residues: Residue<number>[]): void {
	for (const r of residues) {
		if (!r || !r.factor) continue;
		if (r.factor.degree() < 0) continue; // global-resultant placeholder

		if (r.residue) {
			const deg = r.residue.degree();
			assert(deg < r.factor.degree(), `Residue degree ${deg} not reduced modulo factor ${r.factor.toString()}`);
		}

		if (r.residueK) {
			const pk = r.residueK;
			for (const idx in pk.c) {
				const coeff = pk.c[+idx];
				if (!coeff) continue;
				const d = coeff.v.degree();
				assert(d < r.factor.degree(), `ResidueK coeff degree ${d} not reduced modulo factor ${r.factor.toString()}`);
			}
		}
	}
}

const zero = Polynomial([0]);
const one = Polynomial([1]);
const t = Polynomial([0, 1]);

function runCase(name: string, N: Polynomial<Polynomial<number>>, D: Polynomial<Polynomial<number>>): void {
	console.log(`running ${name}`);
	const partial = rothsteinPartial(N, D);
	const residues = rothsteinResidues(N, D, partial.gcds);
	checkResiduesInvariant(residues);
	console.log(`${name} ok`);
}

// Case definitions
(function case1() {
	const D = Polynomial([Polynomial([-1]), zero, one]);
	const N = Polynomial([one]);
	runCase('case1', N, D);
})();

(function case2() {
	const D = Polynomial([zero, one]);
	const N = Polynomial([t]);
	const partial = rothsteinPartial(N, D);
	const residues = rothsteinResidues(N, D, partial.gcds);
	const global = residues.find(r => !r.factor || r.factor.degree() < 0);
	if (global && global.residue) {
		assert(global.residue.map(i => +i).eq(t), `case2: unexpected residue ${global.residue.toString()} !== ${t.toString()}`);
	}
	checkResiduesInvariant(residues);
	console.log('case2 ok');
})();

(function case3() {
	const D = Polynomial([one, zero, one]);
	const N = Polynomial([one]);
	runCase('case3', N, D);
})();

(function case4() {
	const D = Polynomial([Polynomial([-2]), zero, one]);
	const N = Polynomial([one]);
	runCase('case4', N, D);
})();

(function case5() {
	const D = Polynomial([t.neg(), zero, one]);
	const N = Polynomial([one]);
	runCase('case5', N, D);
})();

(function case6() {
	const D = Polynomial([t.mul(t).neg(), one]);
	const N = Polynomial([t]);
	runCase('case6', N, D);
})();

(function case7() {
	const D = Polynomial([t.add(one), zero, one]);
	const N = Polynomial([one]);
	runCase('case7', N, D);
})();

console.log('rothstein-all tests passed');
process.exit(0);