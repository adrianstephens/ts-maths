import { expect, test } from './test';
import {polynomial, aberth, polynomialN} from '../src/polynomial';

//const poly2 = new polynomial([-4, 0, 1]);
const poly2 = new polynomial([-1, 1]).mul(new polynomial([-2, 1])).mul(new polynomial([-3, 1])).mul(new polynomial([-4, 1])).mul(new polynomial([-5, 1]));
const poly3 = new polynomialN([-1]).mul(new polynomialN([-2])).mul(new polynomialN([-3])).mul(new polynomialN([-4])).mul(new polynomialN([-5])).mul(new polynomialN([-6]));
const r = poly2.roots();
console.log(r);

const r1 = poly3.roots();
console.log(r1);

const r2 = aberth(poly2.normalise());
console.log(r2.join('\n'));

const r3 = aberth(poly3);
console.log(r3.join('\n'));
