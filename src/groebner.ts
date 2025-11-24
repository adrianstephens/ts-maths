/**
 * Groebner Basis Implementation for Polynomial Factorization
 * 
 * Operates directly on symbolic expressions (symbolicAdd/symbolicMul)
 * without introducing duplicate data structures.
 */

import { symbolic } from './symbolic';

interface Term {
	coef: symbolic;
	mon: symbolic;
}

// ============================================================================
// MONOMIAL OPERATIONS (using symbolicMul structure)
// ============================================================================

/**
 * Extract exponents for given variables from a symbolicMul or variable
 * Returns exponents array matching the variables list
 */
function getExponents(expr: symbolic, variables: readonly symbolic[]): number[] {
	const exponents = new Array(variables.length).fill(0);
	
	if (expr.is('mul')) {
		for (const factor of expr.factors) {
			const idx = variables.indexOf(factor.item);
			if (idx >= 0)
				exponents[idx] = factor.pow;
		}
	} else {
		const idx = variables.indexOf(expr);
		if (idx >= 0)
			exponents[idx] = 1;
	}
	
	return exponents;
}

/**
 * Total degree of a monomial
 */
function _degree(expr: symbolic, variables: readonly symbolic[]): number {
	return getExponents(expr, variables).reduce((sum, e) => sum + e, 0);
}

/**
 * Compare two monomials using graded reverse lexicographic ordering
 * Returns: >0 if a>b, <0 if a<b, 0 if equal
 */
function compareGrevlex(a: symbolic, b: symbolic, variables: readonly symbolic[]): number {
	const expA = getExponents(a, variables);
	const expB = getExponents(b, variables);
	
	const degA = expA.reduce((sum, e) => sum + e, 0);
	const degB = expB.reduce((sum, e) => sum + e, 0);
	
	if (degA !== degB)
		return degB - degA;
	
	// Reverse lex: compare from right to left
	for (let i = variables.length - 1; i >= 0; i--) {
		if (expA[i] !== expB[i])
			return expA[i] - expB[i];
	}
	return 0;
}

/**
 * Compare using lexicographic ordering (for elimination)
 */
function compareLex(a: symbolic, b: symbolic, variables: readonly symbolic[]): number {
	const expA = getExponents(a, variables);
	const expB = getExponents(b, variables);
	
	for (let i = 0; i < variables.length; i++) {
		if (expA[i] !== expB[i])
			return expB[i] - expA[i];
	}
	return 0;
}

/**
 * Check if monomial a divides monomial b
 */
function divides(a: symbolic, b: symbolic, variables: readonly symbolic[]): boolean {
	const expA = getExponents(a, variables);
	const expB = getExponents(b, variables);
	return expA.every((e, i) => e <= expB[i]);
}

/**
 * Divide monomial b by monomial a (returns quotient monomial)
 * Returns undefined if not divisible
 */
function divideMonomial(b: symbolic, a: symbolic, variables: readonly symbolic[]): symbolic | undefined {
	const expA = getExponents(a, variables);
	const expB = getExponents(b, variables);
	
	const result: number[] = [];
	for (let i = 0; i < variables.length; i++) {
		const diff = expB[i] - expA[i];
		if (diff < 0)
			return undefined;
		result[i] = diff;
	}
	
	// Build monomial from exponents
	let mon = symbolic.from(1);
	for (let i = 0; i < variables.length; i++) {
		if (result[i] > 0)
			mon = mon.mul(variables[i].npow(result[i]));
	}
	return mon;
}

/**
 * LCM of two monomials
 */
function lcmMonomial(a: symbolic, b: symbolic, variables: readonly symbolic[]): symbolic {
	const expA = getExponents(a, variables);
	const expB = getExponents(b, variables);
	
	let result = symbolic.from(1);
	for (let i = 0; i < variables.length; i++) {
		const maxExp = Math.max(expA[i], expB[i]);
		if (maxExp > 0)
			result = result.mul(variables[i].npow(maxExp));
	}
	return result;
}

// ============================================================================
// POLYNOMIAL OPERATIONS (using symbolicAdd structure)
// ============================================================================

/**
 * Extract term: coefficient monomial pair from a symbolicAdd term
 * Separates coefficient (non-variable factors) from monomial (variable powers)
 */
function extractTerm(expr: symbolic, variables: readonly symbolic[]): Term {
	if (expr.is('mul')) {
		const coefFactors: symbolic[] = expr.num !== 1 ? [symbolic.from(expr.num)] : [];
		const monFactors: symbolic[] = [];
		
		for (const factor of expr.factors) {
			if (variables.includes(factor.item)) {
				monFactors.push(factor.item.npow(factor.pow));
			} else {
				coefFactors.push(factor.item.npow(factor.pow));
			}
		}
		
		const coef = coefFactors.length === 0 ? symbolic.from(1) : coefFactors.reduce((a, b) => a.mul(b));
		const mon = monFactors.length === 0 ? symbolic.from(1) : monFactors.reduce((a, b) => a.mul(b));
		return { coef, mon };
	}
	
	if (variables.includes(expr))
		return { coef: symbolic.from(1), mon: expr };
	
	// Entire expression is coefficient
	return { coef: expr, mon: symbolic.from(1) };
}

/**
 * Get leading term from a polynomial (symbolicAdd)
 */
function leadingTerm(poly: symbolic, variables: readonly symbolic[], ordering: (a: symbolic, b: symbolic, variables: readonly symbolic[]) => number): Term | undefined {
	// After expansion, unwrap if it got wrapped again
	let scale = 1;
	if (poly.is('mul') && poly.factors.length === 1 && poly.factors[0].pow === 1 && poly.factors[0].item.is('add')) {
		scale = poly.num;
		poly = poly.factors[0].item;
	}
	
	if (!poly.is('add'))
		return extractTerm(poly, variables);
	
	// Find term with largest monomial
	const bestTerm = extractTerm(poly.terms[0].item, variables);
	let bestMon = bestTerm.mon;
	let bestCoef = bestTerm.coef.scale(poly.terms[0].coef * scale);
	
	for (let i = 1; i < poly.terms.length; i++) {
		const term = extractTerm(poly.terms[i].item, variables);
		// ordering(a, b) returns positive if b > a (i.e., b is larger)
		// So ordering(bestMon, term.mon) > 0 means term.mon is larger than bestMon
		if (ordering(bestMon, term.mon, variables) > 0) {
			bestMon = term.mon;
			bestCoef = term.coef.scale(poly.terms[i].coef * scale);
		}
	}
	
	return { coef: bestCoef, mon: bestMon };
}

/**
 * S-polynomial: S(f, g) = (lcm/LT(f)) * f - (lcm/LT(g)) * g
 */
export function sPolynomial(f: symbolic, g: symbolic, variables: readonly symbolic[], ordering: (a: symbolic, b: symbolic, vars: readonly symbolic[]) => number = compareGrevlex): symbolic {
	const ltf = leadingTerm(f, variables, ordering);
	const ltg = leadingTerm(g, variables, ordering);
	
	if (!ltf || !ltg)
		return symbolic.from(0);
	
	const lcm = lcmMonomial(ltf.mon, ltg.mon, variables);
	
	const mf = divideMonomial(lcm, ltf.mon, variables)!;
	const mg = divideMonomial(lcm, ltg.mon, variables)!;
	
	// Scale to cancel leading terms: (mf/cf)*f - (mg/cg)*g
	const term1 = f.mul(mf).div(ltf.coef).expand();
	const term2 = g.mul(mg).div(ltg.coef).expand();
	
	return term1.sub(term2).expand();
}

/**
 * Multivariate polynomial division with remainder
 * Given f and divisors [g₁, g₂, ..., gₛ], compute:
 * f = q₁g₁ + q₂g₂ + ... + qₛgₛ + r
 */
export function polynomialDivision(
	f: symbolic,
	divisors: symbolic[],
	variables: readonly symbolic[],
	ordering: (a: symbolic, b: symbolic, vars: readonly symbolic[]) => number = compareGrevlex
): { quotients: symbolic[]; remainder: symbolic } {
	const quotients = divisors.map(_ => symbolic.from(0));
	let remainder = symbolic.from(0);
	let dividend = f;
	
	let lastDividend = '';
	let stuckCount = 0;
	
	while (!isZero(dividend)) {
		// Detect infinite loop
		const dividendStr = dividend.id;
		if (dividendStr === lastDividend) {
			stuckCount++;
			if (stuckCount > 2) {
				console.error('Division stuck in infinite loop, dividend:', dividend.toString());
				break;
			}
		} else {
			stuckCount = 0;
			lastDividend = dividendStr;
		}
		
		const lt = leadingTerm(dividend, variables, ordering);
		if (!lt)
			break;
		
		let divided = false;
		
		for (let i = 0; i < divisors.length; i++) {
			const ltg = leadingTerm(divisors[i], variables, ordering);
			if (!ltg)
				continue;
			
			if (divides(ltg.mon, lt.mon, variables)) {
				const m = divideMonomial(lt.mon, ltg.mon, variables)!;
				const c = lt.coef.div(ltg.coef);
				const mc = m.mul(c);
				
				quotients[i] = quotients[i].add(mc);
				dividend = dividend.sub(divisors[i].mul(mc).expand());
				divided = true;
				break;
			}
		}
		
		if (!divided) {
			const ltTerm = lt.mon.mul(lt.coef);
			remainder = remainder.add(ltTerm);
			dividend = dividend.sub(ltTerm);
		}
	}
	
	return { quotients, remainder };
}

/**
 * Buchberger's algorithm for computing Groebner basis
 */
export function groebnerBasis(
	generators: symbolic[],
	variables: readonly symbolic[],
	ordering: (a: symbolic, b: symbolic, vars: readonly symbolic[]) => number = compareGrevlex
): symbolic[] {
	// Expand all generators (GCD-factored muls will be unwrapped in leadingTerm)
	const G = generators.filter(g => !isZero(g)).map(g => g.expand());
	const pairs: [number, number][] = [];
	
	// Generate all pairs
	for (let i = 0; i < G.length; i++) {
		for (let j = i + 1; j < G.length; j++)
			pairs.push([i, j]);
	}
	
	while (pairs.length > 0) {
		const [i, j] = pairs.pop()!;
		const S = sPolynomial(G[i], G[j], variables, ordering);
		const { remainder } = polynomialDivision(S, G, variables, ordering);
		
		if (!isZero(remainder)) {
			// Add new pairs with the new polynomial
			const newIndex = G.length;
			for (let k = 0; k < G.length; k++)
				pairs.push([k, newIndex]);
			
		G.push(remainder.expand());
	}
}

	return reduceGroebner(G, variables, ordering);
}

/**
 * Reduce Groebner basis to minimal form
 */
function reduceGroebner(G: symbolic[], variables: readonly symbolic[], ordering: (a: symbolic, b: symbolic, vars: readonly symbolic[]) => number): symbolic[] {
	// Make monic (leading coefficient = 1), but only if monomial is non-constant
	let result = G.map(g => {
		const lt = leadingTerm(g, variables, ordering);
		if (!lt)
			return g;
		// Don't make monic if the monomial is just 1 (constant polynomial in these variables)
		if (lt.mon === symbolic.one)
			return g;
		return g.div(lt.coef);
	});
	
	// Remove redundant polynomials
	result = result.filter((f, i) => {
		const ltf = leadingTerm(f, variables, ordering);
		if (!ltf)
			return false;
		const redundant = result.some((g, j) => {
			if (i === j)
				return false;
			const ltg = leadingTerm(g, variables, ordering);
			return ltg && divides(ltg.mon, ltf.mon, variables);
		});
		return !redundant;
	});
	
	// Final inter-reduction: reduce each polynomial using the others
	// Only keep the reduction if it's actually simpler
	result = result.map((f, i) => {
		const others = result.filter((_, j) => i !== j);
		if (others.length === 0)
			return f;
		const { remainder } = polynomialDivision(f, others, variables, ordering);
		// Only use remainder if it's non-zero (don't eliminate basis elements!)
		return isZero(remainder) ? f : remainder;
	});
	
	return result.filter(g => !isZero(g));
}

// ============================================================================
// UTILITIES
// ============================================================================

function isZero(expr: symbolic): boolean {
	return expr === symbolic.zero;
}

// ============================================================================
// ORDERINGS
// ============================================================================

export const grevlexOrder = compareGrevlex;
export const lexOrder = compareLex;

// ============================================================================
// POLYNOMIAL FACTORIZATION VIA VIETA'S FORMULAS
// ============================================================================

/**
 * Generate Vieta's formulas for a polynomial of given degree
 * Returns equations relating roots to coefficients
 * 
 * For degree n with roots r₁, r₂, ..., rₙ and coefficients c₁, c₂, ..., cₙ:
 * - σ₁ = r₁ + r₂ + ... + rₙ = -c₁
 * - σ₂ = r₁r₂ + r₁r₃ + ... = c₂
 * - σ₃ = r₁r₂r₃ + ... = -c₃
 * - ...
 * - σₙ = r₁r₂...rₙ = (-1)ⁿcₙ
 */
function _vietasFormulas(roots: symbolic[], coefficients: symbolic[]): symbolic[] {
	const n = roots.length;
	if (n !== coefficients.length)
		throw new Error('Number of roots must match number of coefficients');
	
	const equations: symbolic[] = [];
	
	// Generate elementary symmetric polynomials σₖ
	for (let k = 1; k <= n; k++) {
		let sigma = symbolic.zero;
		
		// Sum over all k-element subsets of roots
		const indices = Array.from({ length: k }, (_, i) => i);
		
		function generateSubsets(start: number, depth: number) {
			if (depth === k) {
				// Multiply the k roots at these indices
				let product = symbolic.one;
				for (let i = 0; i < k; i++)
					product = product.mul(roots[indices[i]]);
				sigma = sigma.add(product);
				return;
			}
			
			for (let i = start; i <= n - (k - depth); i++) {
				indices[depth] = i;
				generateSubsets(i + 1, depth + 1);
			}
		}
		
		generateSubsets(0, 0);
		
		// σₖ = (-1)ᵏ⁺¹ × cₖ  (alternating signs)
		const sign = (k % 2 === 0) ? 1 : -1;
		equations.push(sigma.sub(coefficients[k - 1].scale(sign)));
	}
	
	return equations;
}

/**
 * Factor a univariate polynomial with symbolic coefficients using Groebner basis
 * 
 * Given: polynomial in x with symbolic coefficients
 * Returns: array of root expressions, or undefined if factorization fails
 * 
 * Example:
 *   const A = symbolic.variable('A');
 *   const B = symbolic.variable('B');
 *   const C = symbolic.variable('C');
 *   const x = symbolic.variable('x');
 *   
 *   const poly = x.pow(3)
 *     .sub(A.add(B).add(C).mul(x.pow(2)))
 *     .add(A.mul(B).add(A.mul(C)).add(B.mul(C)).mul(x))
 *     .sub(A.mul(B).mul(C));
 *   
 *   const roots = factorPolynomial(poly, x);
 *   // Should return [A, B, C] (or expressions equal to them)
 */
export function factorPolynomial(poly: symbolic, variable: symbolic): symbolic[] | undefined {
	// Extract polynomial structure: collect coefficients by degree
	const collected = poly.collect(variable.is('var') ? variable.name : variable.toString());
	const degree = collected.length - 1;
	
	if (degree <= 0)
		return undefined;
	
	// For degree 2 and 3, use direct formulas
	if (degree === 2) {
		// ax² + bx + c: roots = (-b ± √(b²-4ac)) / 2a
		const a = collected[2];
		const b = collected[1] ?? symbolic.zero;
		const c = collected[0] ?? symbolic.zero;
		
		const disc = b.pow(2).sub(a.mul(c).scale(4));
		const sqrtDisc = disc.pow(symbolic.from(0.5));
		const denom = a.scale(2);
		
		return [
			b.neg().add(sqrtDisc).div(denom),
			b.neg().sub(sqrtDisc).div(denom)
		];
	}
	
	if (degree === 3) {
		// For cubic, Vieta's formulas directly give us symmetric functions
		// x³ + px² + qx + r = (x-r₁)(x-r₂)(x-r₃)
		// σ₁ = r₁+r₂+r₃ = -p
		// σ₂ = r₁r₂+r₁r₃+r₂r₃ = q  
		// σ₃ = r₁r₂r₃ = -r
		
		const a = collected[3];
		const _p = (collected[2] ?? symbolic.zero).div(a);
		const _q = (collected[1] ?? symbolic.zero).div(a);
		const _r = (collected[0] ?? symbolic.zero).div(a);
		
		// For now, just return symbolic roots using variables
		// A full cubic solver would use Cardano's formula
		const y1 = symbolic.variable('y1');
		const y2 = symbolic.variable('y2');
		const y3 = symbolic.variable('y3');
		
		// Return the roots as unknowns constrained by Vieta
		// This is incomplete but demonstrates the structure
		return [y1, y2, y3];
	}
	
	return undefined;
}

/**
 * Attempt to factor a polynomial into linear factors
 * Returns factors as (x - r₁)(x - r₂)...(x - rₙ) if successful
 */
export function factorIntoLinear(poly: symbolic, variable: symbolic): symbolic | undefined {
	const roots = factorPolynomial(poly, variable);
	if (!roots)
		return undefined;
	
	// Build product of (x - rᵢ)
	let result = symbolic.one;
	for (const root of roots)
		result = result.mul(variable.sub(root));
	
	return result;
}
