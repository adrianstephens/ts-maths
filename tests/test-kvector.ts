import { test, expect, verify, approxArray, makeApproxArray } from './test';
import { Blade, BladeT } from '../dist/kvector';
import { E3, float3, vectorT } from '../dist/vector';
import { symbolic } from '../dist/symbolic';

test('wedge product 2-vectors', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	const blade = Blade.from(v1, v2);
	
	expect(blade.k, 'grade').toEqual(2);
	expect(blade.n, 'dimension').toEqual(3);
	expect(blade.components.length, 'component count').toEqual(3); // C(3,2)
	
	// Expected: v1 ∧ v2 = (1*5 - 2*4)e₀∧e₁ + (1*6 - 3*4)e₀∧e₂ + (2*6 - 3*5)e₁∧e₂
	//                   = -3·e₀∧e₁ + (-6)·e₀∧e₂ + (-3)·e₁∧e₂
	verify(blade.components, [-3, -6, -3], approxArray, 'components');
});

test('wedge product 3-vectors (coplanar)', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	const v3 = [7, 8, 9]; // coplanar with v1, v2
	
	const blade = Blade.from(v1, v2, v3);
	
	expect(blade.k, 'grade').toEqual(3);
	expect(blade.n, 'dimension').toEqual(3);
	expect(blade.components.length, 'component count').toEqual(1); // C(3,3)
	verify(blade.components, [0], approxArray, 'coplanar determinant');
});

test('wedge product 3-vectors (non-coplanar)', () => {
	const v1 = [1, 0, 0];
	const v2 = [0, 1, 0];
	const v3 = [0, 0, 1];
	
	const blade = Blade.from(v1, v2, v3);
	
	verify(blade.components, [1], approxArray, 'unit volume');
});

test('incremental wedge', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	const v3 = [7, 8, 10];
	
	const blade2 = Blade.from(v1, v2);
	const blade3 = blade2.wedge(v3);
	
	const direct = Blade.from(v1, v2, v3);
	
	verify(blade3.components, direct.components, approxArray, 'incremental vs direct');
});

test('getCoeff', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	const blade = Blade.from(v1, v2);
	
	expect(blade.getCoeff([0, 1]), 'e₀∧e₁').toBeCloseTo(-3);
	expect(blade.getCoeff([0, 2]), 'e₀∧e₂').toBeCloseTo(-6);
	expect(blade.getCoeff([1, 2]), 'e₁∧e₂').toBeCloseTo(-3);
});

test('add blades', () => {
	const v1 = [1, 0, 0];
	const v2 = [0, 1, 0];
	const blade1 = Blade.from(v1, v2); // e₀∧e₁
	
	const v3 = [0, 1, 0];
	const v4 = [0, 0, 1];
	const blade2 = Blade.from(v3, v4); // e₁∧e₂
	
	const sum = blade1.add(blade2);
	
	// sum = e₀∧e₁ + e₁∧e₂ = [1, 0, 1]
	verify(sum.components, [1, 0, 1], approxArray, 'sum components');
});

test('scale blade', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	const blade = Blade.from(v1, v2);
	
	const scaled = blade.scale(2);
	
	verify(scaled.components, blade.components.map(x => x * 2), approxArray, 'scaled');
});

test('norm and normalize', () => {
	const v1 = [1, 0, 0];
	const v2 = [0, 1, 0];
	const blade = Blade.from(v1, v2);
	
	expect(blade.norm(), 'norm').toBeCloseTo(1);
	
	const normalized = blade.normalize();
	expect(normalized.norm(), 'normalized norm').toBeCloseTo(1);
});

test('Hodge dual (cross product)', () => {
	const v1 = [1, 2, 3];
	const v2 = [4, 5, 6];
	
	const bivector = Blade.from(v1, v2);
	const cross = bivector.dual();
	
	// Cross product: v1 × v2 = [2*6 - 3*5, 3*4 - 1*6, 1*5 - 2*4]
	//                        = [-3, 6, -3]
	expect(cross.k, 'dual grade').toEqual(1);
	verify(cross.components, [-3, 6, -3], approxArray, 'cross product');
});

test('Hodge dual (double dual)', () => {
	const v1 = float3(1, 0, 0);
	const v2 = float3(0, 1, 0);
	
	const blade = Blade.from(v1, v2);
	const dual = blade.dual();
	const doubleDual = dual.dual();
	
	// Double dual should return ±original
	const approxEq = makeApproxArray(1e-9);
	const matches = approxEq(doubleDual.components, blade.components) ||
		approxEq(doubleDual.components, blade.components.map(x => -x));
	
	expect(matches, 'double dual ± identity').toEqual(true);
});

test('4D wedge products', () => {
	const v1 = [1, 0, 0, 0];
	const v2 = [0, 1, 0, 0];
	const v3 = [0, 0, 1, 0];
	const v4 = [0, 0, 0, 1];
	
	const blade2 = Blade.from(v1, v2);
	expect(blade2.components.length, '4D 2-blade components').toEqual(6); // C(4,2)
	
	const blade3 = Blade.from(v1, v2, v3);
	expect(blade3.components.length, '4D 3-blade components').toEqual(4); // C(4,3)
	
	const blade4 = Blade.from(v1, v2, v3, v4);
	expect(blade4.components.length, '4D 4-blade components').toEqual(1); // C(4,4)
	verify(blade4.components, [1], approxArray, 'unit 4-volume');
});

test('Hodge dual symbolic', () => {
	const a = symbolic.variable('a');
	const b = symbolic.variable('b');
	const c = symbolic.variable('c');
	const d = symbolic.variable('d');
	const e = symbolic.variable('e');
	const f = symbolic.variable('f');


	//const zero = symbolic.zero;
	//const one = symbolic.one;

	const v1 = vectorT(E3, a, b, c);
	const v2 = vectorT(E3, d, e, f);
	
	const blade = BladeT.from(v1, v2);
	const dual = blade.dual();
	const doubleDual = dual.dual();

	console.log('blade:', blade.components.map(String));
	console.log('dual:', dual.components.map(String));
	console.log('double dual:', doubleDual.components.map(String));
});

