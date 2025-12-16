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
	expect(eqn).toEqual(complex(3, 4));
	
});


test('complex intrinsic functions', () => {
	const a = complex(1, 2);
	// exp
	expect(complex.exp(a)).toBeCloseTo(complex(-1.1312043837568135, 2.4717266720048188), 1e-12);
	// ln
	expect(complex.ln(a)).toBeCloseTo(complex(0.8047189562170501, 1.1071487177940904), 1e-12);
	// sqrt
	expect(complex.sqrt(a)).toBeCloseTo(complex(1.272019649514069, 0.7861513777574233), 1e-12);
	// sin
	expect(complex.sin(a)).toBeCloseTo(complex(3.165778513216168, 1.9596010414216063), 1e-12);
	// cos
	expect(complex.cos(a)).toBeCloseTo(complex(2.0327230070196656, -3.0518977991518), 1e-12);
	// tan
	expect(complex.tan(a)).toBeCloseTo(complex(0.0338128260798967, 1.0147936161466335), 1e-12);
	// sinh
	expect(complex.sinh(a)).toBeCloseTo(complex(-0.4890562590412937, 1.4031192506220405), 1e-12);
	// cosh
	expect(complex.cosh(a)).toBeCloseTo(complex(-0.6421481247155201, 1.0686074213827783), 1e-12);
	// tanh
	expect(complex.tanh(a)).toBeCloseTo(complex(1.16673625724092, -0.243458201185725), 1e-10);
});
