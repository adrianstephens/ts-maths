/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approxArray, makeApproxArray, sequence } from './test';
import complex, {complexT} from '../dist/complex';
import { parse } from '../dist/string';


test('complex arithmetic', () => {
	const a = complex(3,4);
	const b = complex(2,3);
	expect(a.add(b)).toEqual(complex(5, 7));
	expect(a.sub(b)).toEqual(complex(1, 1));
	expect(a.mul(b)).toEqual(complex(-6, 17));
	expect(complex(-6, 17).div(b)).toEqual(a);
//	expect(a.mod(b)).toEqual(complex(3, 4));

	const eqn = parse(complex, '3 + 4i');
	
});
