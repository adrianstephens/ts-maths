"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const polynomial_1 = require("../src/polynomial");
const factors_1 = require("../src/factors");
function fail(msg, ...rest) {
    console.error(msg, ...rest);
    process.exit(2);
}
// f = x^2 - 1 = (x-1)*(x+1)
const f = (0, polynomial_1.Polynomial)([-1, 0, 1]);
console.log('f:', f.toString(), 'deg=', f.degree());
const factors = (0, factors_1.factorOverK)(f);
console.log('factorOverK result:', factors.map(x => ({ factor: x.factor.toString(), multiplicity: x.multiplicity })));
if (!factors || factors.length !== 2)
    fail('factorOverK returned wrong number of factors', String(factors));
for (const fac of factors) {
    if (fac.factor.degree() !== 1)
        fail('expected linear factor', fac.factor.toString());
}
console.log('maths factorOverK regression test passed');
//# sourceMappingURL=test-factorOverK.js.map