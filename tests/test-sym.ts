import { test, expect, assert } from './test';
import { makeExprOpsAdapter, exprVariable, exprFromNumber } from "../src/symbolic";

test('symbolic instance adapter basic', () => {
    const O = makeExprOpsAdapter();

    const x = exprVariable("x");
    const y = exprVariable("y");
    const two = exprFromNumber(2);

    // build expression (x + y) * 2 using the instance-style ops
    const sum = O.add(x, y);
    const expr = O.mul(sum, two);

    // string form (non-guaranteed formatting, but check tokens present)
    const s = O.toString(expr);
    expect(s, 'string contains x,y,2').check(str => typeof str === 'string' && str.includes('x') && str.includes('y') && str.includes('2'));

    // evaluate at x=3,y=4 -> (3+4)*2 = 14
    const val = O.evaluate(expr, { x: 3, y: 4 });
    expect(val, 'evaluate (3+4)*2').toEqual(14);

    // derivative w.r.t x should be 2
    const d = O.derivative(expr, "x");
    const dv = O.evaluate(d);
    expect(dv, 'derivative d/dx').toEqual(2);
});
