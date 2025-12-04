import { parse } from './dist/string';
import { symbolic, symbolicOperators } from './dist/symbolic';
import { applyRulesEgraph } from './dist/egraph';
import { generalRules } from './dist/symbolicRules';

// same validation used in tests
const bindings = { a: 1, b: 2, c: 3, d: 4 };
const validate = (a: symbolic, b: symbolic) => {
	const av = a.evaluate(bindings);
	const bv = b.evaluate(bindings);
	return av == bv;
};
const opts = {maxRounds: 8, verbose: true, maxExpansions: 16, validate, debugNode: 'replace'};

const exprStr = '(b * c / (a * d - b * c) / a + 1 / a) * b - b * d / (a * d - b * c)';
const node = parse(symbolicOperators, exprStr);
console.log('Parsed AST :', String(node));

const simp = applyRulesEgraph(node, generalRules, opts);
console.log('Simplified :', String(simp));
console.log('Is exact zero (=== symbolic.zero)?', simp === symbolic.zero);
console.log('Evaluated original:', node.evaluate(bindings));
console.log('Evaluated simplified:', simp.evaluate(bindings));

// Print a more verbose form
console.log('Simplified repr:', simp.toString());

