// Wedge product (exterior product) - clean, efficient implementation
import { vscalar, vops } from './vector';
import { toSubscript } from './string';

export interface Blade<T> {
	k: number;
	n: number;
	components: T[];

	wedge(v: T[] | vops<any, number>): Blade<T>;
	getCoeff(indices: number[]): T;
	add(other: Blade<T>):	Blade<T>;
	sub(other: Blade<T>):	Blade<T>;
	scale(scalar: number):	Blade<T>
	norm():					T;
	normalize():			Blade<T>;
	dual():					Blade<T>;
}

// Overloads to ensure numeric literal arrays like `[-1, 1]` resolve to `Polynomial<number>`
export function Blade(...vectors: (number[] | vops<any, number>)[]): Blade<number>;
export function Blade<T extends vscalar<T>>(...vectors: T[][] | vops<any, T>[]): Blade<T>;
export function Blade(...vectors: any[]) {
	switch (typeof vectors[0][0]) {
		case 'number':
			return BladeN.from(...vectors) as Blade<number>;
		default:
			return BladeT.from(...vectors) as Blade<vscalar<any>>;
	}
}

class BladeN {
	constructor(public k: number, public n: number, public components: number[]) {
	}
	static from(...vectors: (number[] | vops<any, number>)[]): BladeN {
		const vecs: number[][] = vectors[0] instanceof Array ? vectors as number[][] : (vectors as vops<any, number>[]).map(v => v._values);
		const k = vecs.length;
		const n = vecs[0].length;
		return new BladeN(k, n, wedge(k, n, vecs));
	}
	wedge(v: number[] | vops<any, number>): BladeN {
		return new BladeN(this.k + 1, this.n, wedgeWith(this.k, this.n, this.components, v instanceof Array ? v : v._values));
	}
	getCoeff(indices: number[]): number {
		return this.components[basisIndex(indices, this.n)];
	}
	add(other: BladeN): BladeN {
		if (this.k !== other.k || this.n !== other.n)
			throw new Error('Blades must have same grade and dimension');
		return new BladeN(this.k, this.n, this.components.map((c, i) => c + other.components[i]));
	}
	sub(other: BladeN): BladeN {
		if (this.k !== other.k || this.n !== other.n)
			throw new Error('Blades must have same grade and dimension');
		return new BladeN(this.k, this.n, this.components.map((c, i) => c - other.components[i]));
	}
	scale(scalar: number): BladeN {
		return new BladeN(this.k, this.n, this.components.map(c => c * scalar));
	}
	norm(): number {
		return Math.sqrt(this.components.reduce((sum, c) => sum + c * c, 0));
	}
	normalize(): BladeN {
		const n = this.norm();
		return n === 0 ? this : this.scale(1 / n);
	}
	dual(): BladeN {
		return new BladeN(this.n - this.k, this.n, dual(this.k, this.n, this.components));
	}
	toString(): string {
		return toString(this.k, this.n, this.components);
	}
}

class BladeT<T extends vscalar<T>> {
	constructor(public k: number, public n: number, public components: T[]) {
	}
	static from<T extends vscalar<T>>(...vectors: T[][] | vops<any, T>[]): BladeT<T> {
		const vecs: T[][] = vectors[0] instanceof Array ? vectors as T[][] : (vectors as vops<any, T>[]).map(v => v._values);
		const k = vecs.length;
		const n = vecs[0].length;
		return new BladeT<T>(k, n, wedgeT(k, n, vecs));
	}
	wedge(v: T[] | vops<any, T>): BladeT<T> {
		return new BladeT(this.k + 1, this.n, wedgeWithT(this.k, this.n, this.components, v instanceof Array ? v : v._values));
	}
	getCoeff(indices: number[]): T {
		return this.components[basisIndex(indices, this.n)];
	}
	add(other: BladeT<T>): BladeT<T> {
		if (this.k !== other.k || this.n !== other.n)
			throw new Error('Blades must have same grade and dimension');
		return new BladeT(this.k, this.n, this.components.map((c, i) => c.add(other.components[i])));
	}
	sub(other: BladeT<T>): BladeT<T> {
		if (this.k !== other.k || this.n !== other.n)
			throw new Error('Blades must have same grade and dimension');
		return new BladeT(this.k, this.n, this.components.map((c, i) => c.sub(other.components[i])));
	}
	scale(scalar: number): BladeT<T> {
		return new BladeT(this.k, this.n, this.components.map(c => c.scale(scalar)));
	}
	norm(): T {
		return this.components.reduce((sum, c) => sum.add(c.mul(c)), this.components[0].from(0)).sqrt();
	}
	normalize(): BladeT<T> {
		const n = +this.norm();
		return n === 0 ? this : this.scale(1 / n);
	}
	dual(): BladeT<T> {
		return new BladeT<T>(this.n - this.k, this.n, dualT(this.k, this.n, this.components));
	}
	toString(): string {
		return toString(this.k, this.n, this.components);
	}
}

function toString(k: number, n: number, components: any[]): string {
	//return `Blade(k=${this.k}, n=${this.n}, components=[${this.components.join(', ')}])`;
	const terms: string[] = [];
	let i = 0;
	for (const basis of combinations(n, k)) {
		//const coeff = components[i++];
		//if (coeff !== 0) {
			const basisStr = basis.map(j => `e${toSubscript(String(j))}`).join('∧');
			const coeffStr = String(components[i++]);
			if (coeffStr.includes(' '))
				terms.push(`(${coeffStr})${basisStr}`);
			else
				terms.push(`${coeffStr}·${basisStr}`);
		//}
	}
	//return terms.length > 0 ? terms.join(' + ') : '0';
	return terms.join(' + ');
}

// Generate all k-combinations of [0..n-1] in lexicographic order
function* combinations(n: number, k: number): Generator<number[]> {
	if (k === 0) {
		yield [];
		return;
	}
	if (k > n)
		return;

	const indices = Array.from({length: k}, (_, i) => i);
	yield [...indices];

	while (true) {
		let i = k - 1;
		while (i >= 0 && indices[i] === n - k + i)
			i--;
		if (i < 0)
			break;

		indices[i]++;
		for (let j = i + 1; j < k; j++)
			indices[j] = indices[j - 1] + 1;
		yield [...indices];
	}
}

// Get coefficient for a specific basis blade from a k-vector
// indices must be sorted: [i₁, i₂, ..., iₖ] where i₁ < i₂ < ... < iₖ
function basisIndex(indices: number[], n: number): number {
	const k = indices.length;
	let index = 0;
	for (let i = 0; i < k; i++) {
		// Count combinations with smaller values at position i
		for (let j = (i === 0 ? 0 : indices[i-1] + 1); j < indices[i]; j++) {
			// C(n - j - 1, k - i - 1)
			let count = 1;
			const remaining = k - i - 1;
			for (let r = 0; r < remaining; r++)
				count = count * (n - j - 1 - r) / (r + 1);
			index += count;
		}
	}
	return index;
}

// Compute determinant of k×k submatrix
function minor(matrix: number[][], cols: number[], rows: number[]): number {
	const col0 = matrix[cols[0]];
	const k = cols.length;
	if (k === 1)
		return col0[rows[0]];
	if (k === 2)
		return col0[rows[0]] * matrix[cols[1]][rows[1]] - col0[rows[1]] * matrix[cols[1]][rows[0]];

	let sum = 0;
	for (let j = 0; j < k; j++)
		sum += (j % 2 === 0 ? 1 : -1) * col0[rows[j]] * minor(matrix, cols.slice(1), [...rows.slice(0, j), ...rows.slice(j + 1)]);
	return sum;
}

// Wedge product: v1 ∧ v2 ∧ ... ∧ vk
// Returns array of k-vector components in lexicographic basis order
function wedge(k: number, n: number, vectors: number[][]): number[] {
	// Vectors are already column-major, compute all k×k minors
	const result: number[] = [];
	const colIndices = Array.from({length: k}, (_, i) => i);
	for (const rowIndices of combinations(n, k))
		result.push(minor(vectors, colIndices, rowIndices));

	return result;
}

// Wedge blade with vector: (k-blade) ∧ v → (k+1)-blade
function wedgeWith(k: number, n: number, components: number[], v: number[]) {
	const result: number[] = [];

	// Precompute all k-combinations (indices for blade components)
	const oldCombos = Array.from(combinations(n, k));

	// For each (k+1)-combination (new blade component)
	for (const newCombo of combinations(n, k + 1)) {
		let sum = 0;

		// Expand determinant along v's column: Σ (-1)^r * v[newCombo[r]] * minor_r where minor_r is the k×k determinant with row newCombo[r] removed
		for (let r = 0; r < k + 1; r++) {
			const removedRow = newCombo[r];
			const remainingRows = [...newCombo.slice(0, r), ...newCombo.slice(r + 1)];

			// Find which blade component corresponds to these rows
			const oldIdx = oldCombos.findIndex(combo => combo.length === remainingRows.length && combo.every((val, i) => val === remainingRows[i]));
			if (oldIdx >= 0)
				sum += (r % 2 === 0 ? 1 : -1) * v[removedRow] * components[oldIdx];
		}
		result.push(sum);
	}
	return result;
}

function dual(k: number, n: number, components: number[]) {
	const result: number[] = [];
	
	// For each basis element of the dual
	for (const dualBasis of combinations(n, n - k)) {
		// Find complement basis (what this dual basis wedges with to get pseudoscalar)
		const complement = Array.from({length: n}, (_, i) => i).filter(i => !dualBasis.includes(i));
		
		// Compute sign from permutation
		const allIndices = [...complement, ...dualBasis];
		let sign = 1;
		for (let i = 0; i < allIndices.length; i++)
			for (let j = i + 1; j < allIndices.length; j++)
				if (allIndices[i] > allIndices[j])
					sign *= -1;
		
		result.push(sign * components[basisIndex(complement, n)]);
	}
	return result;
}

// Compute determinant of k×k submatrix
function minorT<T extends vscalar<T>>(matrix: T[][], cols: number[], rows: number[], zero: T): T {
	const col0 = matrix[cols[0]];
	const k = cols.length;
	if (k === 1)
		return col0[rows[0]];
	if (k === 2)
		return col0[rows[0]].mul(matrix[cols[1]][rows[1]]).sub(col0[rows[1]].mul(matrix[cols[1]][rows[0]]));

	let sum = zero;
	for (let j = 0; j < k; j++)
		sum = sum.add((j % 2 === 0 ? col0[rows[j]] : col0[rows[j]].neg()).mul(minorT(matrix, cols.slice(1), [...rows.slice(0, j), ...rows.slice(j + 1)], zero)));
	return sum;
}

// Wedge product: v1 ∧ v2 ∧ ... ∧ vk
// Returns array of k-vector components in lexicographic basis order
function wedgeT<T extends vscalar<T>>(k: number, n: number, vectors: T[][]): T[] {
	// Vectors are already column-major, compute all k×k minors
	const result: T[] = [];
	const colIndices = Array.from({length: k}, (_, i) => i);
	const zero = vectors[0][0].from(0);
	
	for (const rowIndices of combinations(n, k))
		result.push(minorT(vectors, colIndices, rowIndices, zero));

	return result;
}


// Wedge blade with vector: (k-blade) ∧ v → (k+1)-blade
function wedgeWithT<T extends vscalar<T>>(k: number, n: number, components: T[], v: T[]) {
	const result: T[] = [];
	const zero = v[0].from(0);

	// Precompute all k-combinations (indices for blade components)
	const oldCombos = Array.from(combinations(n, k));

	// For each (k+1)-combination (new blade component)
	for (const newCombo of combinations(n, k + 1)) {
		let sum = zero;

		// Expand determinant along v's column: Σ (-1)^r * v[newCombo[r]] * minor_r where minor_r is the k×k determinant with row newCombo[r] removed
		for (let r = 0; r < k + 1; r++) {
			const removedRow = newCombo[r];
			const remainingRows = [...newCombo.slice(0, r), ...newCombo.slice(r + 1)];

			// Find which blade component corresponds to these rows
			const oldIdx = oldCombos.findIndex(combo => combo.length === remainingRows.length && combo.every((val, i) => val === remainingRows[i]));
			if (oldIdx >= 0)
				sum = sum.add((r % 2 === 0 ? v[removedRow] : v[removedRow].neg()).mul(components[oldIdx]));
		}

		result.push(sum);
	}
	return result;
}

function dualT<T extends vscalar<T>>(k: number, n: number, components: T[]) {
	const result: T[] = [];
	
	// For each basis element of the dual
	for (const dualBasis of combinations(n, n - k)) {
		// Find complement basis (what this dual basis wedges with to get pseudoscalar)
		const complement = Array.from({length: n}, (_, i) => i).filter(i => !dualBasis.includes(i));
		
		// Compute sign from permutation
		const allIndices = [...complement, ...dualBasis];
		let neg = false;
		for (let i = 0; i < allIndices.length; i++)
			for (let j = i + 1; j < allIndices.length; j++)
				if (allIndices[i] > allIndices[j])
					neg = !neg;
		
		const v = components[basisIndex(complement, n)];
		result.push(neg ? v.neg() : v);
	}
	
	return result;
}
