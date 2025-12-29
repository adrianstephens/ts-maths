import { Polynomial } from '../src/polynomial';
import { factorOverK } from '../src/factors';

function fail(msg: string, ...rest: string[]) {
  console.error(msg, ...rest);
  process.exit(2);
}

// f = x^2 - 1 = (x-1)*(x+1)
const f = Polynomial([-1, 0, 1]);
console.log('f:', f.toString(), 'deg=', f.degree());
const factors = factorOverK(f);
console.log('factorOverK result:', factors.map(x => ({ factor: x.factor.toString(), multiplicity: x.multiplicity })));
if (!factors || factors.length !== 2) fail('factorOverK returned wrong number of factors', String(factors));
for (const fac of factors) {
  if (fac.factor.degree() !== 1) fail('expected linear factor', fac.factor.toString());
}

console.log('maths factorOverK regression test passed');
