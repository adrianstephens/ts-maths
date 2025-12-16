/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approxArray, makeApproxArray, sequence } from './test';
import rational from '../dist/rational';
import { parse } from '../dist/string';

test('rational arithmetic', () => {
	const a = rational(3,4);
	const b = rational(2,3);
	expect(a.add(b)).toEqual(rational(17, 12));
	expect(a.sub(b)).toEqual(rational(1, 12));
	expect(a.mul(b)).toEqual(rational(1, 2));
	expect(a.div(b)).toEqual(rational(9, 8));
	expect(a.mod(b)).toEqual(rational(1, 12));//1/8 * 2/3

	const eqn = parse(rational, '3 / 4');
	expect(eqn).toEqual(rational(3, 4));
	
});
