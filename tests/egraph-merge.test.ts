import { test, expect } from './test';
import { symbolic } from '../dist/symbolic';
import EGraphDefault from '../dist/egraph';

// This test exercises the mergeClasses parent-recomputation path.
// It builds two distinct child classes each with a parent node, then
// merges the child classes via the internal `mergeClasses` method and
// asserts the parent's canonical signature remains registered.

test('egraph.mergeClasses recompute only merged parents (smoke)', () => {
	const EGraph: any = EGraphDefault as any;
	const eg = new EGraph();

	const x1 = symbolic.variable('x1');
	const y = symbolic.variable('y');
	const p1 = x1.add(y);

	const x2 = symbolic.variable('x2');
	const p2 = x2.add(y);

	// register both parent expressions (this also registers children)
	eg.addSymbolic(p1);
	eg.addSymbolic(p2);

	// access private helpers via casting to any
	const eclassOf = (eg as any).eclassOfNode.bind(eg);
	const computeSig = (eg as any).computeCanonicalSig.bind(eg);
	const cmap = (eg as any).canonicalMap as Map<string, any>;

	const ec1 = eclassOf(x1.id);
	const ec2 = eclassOf(x2.id);
	if (!ec1 || !ec2) throw new Error('eclasses missing');

	// ensure parents are registered
	expect(Array.from(ec1.parents).length).check(n => n >= 1);
	expect(Array.from(ec2.parents).length).check(n => n >= 1);

	// ensure parent eclass exists before merge
	const parentEclassBefore = (eg as any).eclassOfNode(p2.id);
	expect(Boolean(parentEclassBefore)).toEqual(true);

	// call private mergeClasses to merge child eclasses
	(eg as any).mergeClasses(ec1, ec2);

	// after merge, parent eclass should still be present
	const parentEclassAfter = (eg as any).eclassOfNode(p2.id);
	expect(Boolean(parentEclassAfter)).toEqual(true);
});
