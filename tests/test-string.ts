/* eslint-disable @typescript-eslint/no-unused-vars */
import { test, expect, assert } from './test';
import { Operators } from '../dist/core';
import Num from '../dist/num';
import { OperatorsBase } from '../dist/gen';
import { outputNumber, radicalChars, fractionChars, parse } from '../dist/string';
import { symbolic, symbolicOperators } from '../dist/symbolic';
import big from '../../big/dist/index';

//symbolic.setDefaultStringifyOptions({ccode: true, radicalPower: true, superPower: true});
symbolic.setDefaultStringifyOptions({
	superPower: true,
	radicalPower: true,
	mulChar: '⋅',
	printConst: n=>outputNumber(n, {fractions: {chars: fractionChars, superSub: true}, radicals: radicalChars})}
);


const bigOperators: Operators<big> = {
	...OperatorsBase(big),
	from(n: number) { return big.from(n, 100); },
	variable(name: string) {
		switch (name) {
			case 'pi':			return big.pi(100);
			case 'e':			return big.exp(1, 100);
			case 'infinity':	return big.Infinity;
			default:			return undefined;
		}
	},
};


test('string', () => {
	const equation = 'sin(pi / 4) + log(10) sqrt(2)';
	const resn = parse(Num, equation);
	const resb = parse(bigOperators, equation);
	expect(resn).toBeCloseTo(Math.sin(Math.PI / 4) + Math.log(10) * Math.sqrt(2));

	expect(parse(Num, '∛⅒')).toBeCloseTo(0.1 ** (1 / 3));
	expect(outputNumber(0.1 ** (1 / 3))).toEqual('∛⅒');
	expect(parse(Num, '⁶⁷/₂₃₄')).toBeCloseTo(67/234);

	const pow = parse(symbolicOperators, '(x^2+y^2+z^2)^x');
	const dpow = pow.derivative("x");
	expect(dpow).toEqual(parse(symbolicOperators, '2x²(x² + y² + z²)^(x - 1) + log(x² + y² + z²)(x² + y² + z²)^x'));
	expect(dpow).toEqual(parse(symbolicOperators, '2x.x(x² + y² + z²)^(x - 1) + log(x² + y² + z²)(x² + y² + z²)^x'));
//	expect(dpow).toEqual(parse(symbolicOperators, '2 * x² * (x² + y² + z²) ^ (x - 1) + log(x² + y² + z²) * (x² + y² + z²) ^ (x)'));
});
