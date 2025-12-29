import { Polynomial } from '../src/polynomial';
import { squareFreeFactorization, polyGCD } from '../src/factors';

function fail(msg: string, ...rest: any[]) {
  console.error(msg, ...rest);
  process.exit(2);
}

// f = x^2 - 2x + 1 = (x-1)^2
const f = Polynomial([1, -2, 1]);
console.log('f:', f.toString(), 'deg=', f.degree());
const fder = f.deriv();
console.log('f\' deriv:', fder.toString(), 'deg=', fder.degree());
const g = polyGCD(f, fder);
console.log('gcd:', g.toString(), 'deg=', g.degree());
if (g.degree() !== 1) fail('gcd degree unexpected', g.toString());

const factors = squareFreeFactorization(f);
console.log('squareFreeFactorization result:', factors);
if (!factors || factors.length !== 1) fail('squareFreeFactorization returned wrong number of factors', factors);
const fac = factors[0];
if (fac.multiplicity !== 2) fail('expected multiplicity 2, got', fac.multiplicity, fac.factor.toString());

console.log('maths square-free regression test passed');
