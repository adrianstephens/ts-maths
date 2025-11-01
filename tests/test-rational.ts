/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approx, approxArray, makeApproxArray, sequence } from './test';
import {rational, rationalB} from '../dist/rational';
import * as big from '../../big/dist/index';


test('rational arithmetic', () => {
	const a = new rational(3,4);
	const b = new rational(2,3);
	expect(a.add(b).toString(), '3/4 + 2/3').toEqual('17 / 12');
	expect(a.sub(b).toString(), '3/4 - 2/3').toEqual('1 / 12');
	expect(a.mul(b).toString(), '3/4 * 2/3').toEqual('1 / 2');
	expect(a.div(b).toString(), '3/4 / 2/3').toEqual('9 / 8');//or 1 + 1/8
	expect(a.mod(b).toString(), '3/4 % 2/3').toEqual('1 / 12');//1/8 * 2/3
});
