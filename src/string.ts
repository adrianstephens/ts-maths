import { Num, Operators, isAlmostInteger } from './core';
import rational from './rational';
import algebraic from './algebraic';

//-----------------------------------------------------------------------------
// output
//-----------------------------------------------------------------------------

type FractionOptions = false | { chars?: Record<number, Record<number, string>>; superSub?: boolean };

type ConstOptions = {
	fractions?: FractionOptions;
	radicals?: Record<number, string>;
};

const superscriptMap: Record<string, string> = {
	'0': '⁰', '1': '¹', '2': '²', '3': '³', '4': '⁴', '5': '⁵', '6': '⁶', '7': '⁷', '8': '⁸', '9': '⁹',
	'₀': '⁰', '₁': '¹', '₂': '²', '₃': '³', '₄': '⁴', '₅': '⁵', '₆': '⁶', '₇': '⁷', '₈': '⁸', '₉': '⁹',
	'+': '⁺', '-': '⁻', '=': '⁼', '(': '⁽', ')': '⁾', '/': 'ᐟ', '⁄': 'ᐟ', '.': '˙',
	'a': 'ᵃ', 'b': 'ᵇ', 'c': 'ᶜ', 'd': 'ᵈ', 'e': 'ᵉ', 'f': 'ᶠ', 'g': 'ᵍ', 'h': 'ʰ', 'i': 'ᶦ', 'j': 'ʲ', 'k': 'ᵏ', 'l': 'ˡ',
	'm': 'ᵐ', 'n': 'ⁿ', 'o': 'ᵒ', 'p': 'ᵖ', 'r': 'ʳ', 's': 'ˢ', 't': 'ᵗ', 'u': 'ᵘ', 'v': 'ᵛ', 'w': 'ʷ', 'x': 'ˣ', 'y': 'ʸ', 'z': 'ᶻ',

	'A': 'ᴬ', 'B': 'ᴮ', 'D': 'ᴰ', 'E': 'ᴱ', 'G': 'ᴳ', 'H': 'ᴴ', 'I': 'ᴵ', 'J': 'ᴶ', 'K': 'ᴷ', 'L': 'ᴸ',
	'M': 'ᴹ', 'N': 'ᴺ', 'O': 'ᴼ', 'P': 'ᴾ', 'R': 'ᴿ', 'T': 'ᵀ', 'U': 'ᵁ', 'W': 'ᵂ',

	'½': '¹ᐟ²', '⅓': '¹ᐟ³', '⅔': '²ᐟ³', '¼': '¹ᐟ⁴', '¾': '³ᐟ⁴',
	'⅕': '¹ᐟ⁵', '⅖': '²ᐟ⁵', '⅗': '³ᐟ⁵', '⅘': '⁴ᐟ⁵',
	'⅙': '¹ᐟ⁶', '⅚': '⁵ᐟ⁶',
	'⅐': '¹ᐟ⁷',
	'⅛': '¹ᐟ⁸', '⅜': '³ᐟ⁸', '⅝': '⁵ᐟ⁸', '⅞': '⁷ᐟ⁸',
	'⅑': '¹ᐟ⁹',
	'⅒': '¹ᐟ¹⁰',
};

const subscriptMap: Record<string, string> = {
	'0': '₀', '1': '₁', '2': '₂', '3': '₃', '4': '₄', '5': '₅', '6': '₆', '7': '₇', '8': '₈', '9': '₉',
	'+': '₊', '-': '₋', '=': '₌', '(': '₍', ')': '₎',

	'a': 'ₐ', 'e': 'ₑ', 'o': 'ₒ', 'x': 'ₓ', 'h': 'ₕ', 'k': 'ₖ', 'l': 'ₗ', 'm': 'ₘ', 'n': 'ₙ', 'p': 'ₚ', 's': 'ₛ', 't': 'ₜ'
};

export const fractionChars: Record<number, Record<number, string>> = {
	'2': {'1': '½'},
	'3': {'1': '⅓', '2': '⅔'},
	'4': {'1': '¼', '3': '¾'},
	'5': {'1': '⅕', '2': '⅖', '3': '⅗', '4': '⅘'},
	'6': {'1': '⅙', '5': '⅚'},
	'7': {'1': '⅐'},
	'8': {'1': '⅛', '3': '⅜', '5': '⅝', '7': '⅞'},
	'9': {'1': '⅑'},
	'10': {'1': '⅒'},
};

export const radicalChars: Record<number, string> = {'2': '√', '3': '∛', '4': '∜'};

function buildReverseMap(map: Record<string, string>): Record<string, string> {
	return Object.entries(map).reduce((acc, [k, v]) => {
		if (v.length === 1 && !acc[v])
			acc[v] = k;
		return acc;
	}, {} as Record<string, string>);
}

const revFractionChars = Object.entries(fractionChars).reduce((acc, [den, numMap]) => {
	for (const [num, char] of Object.entries(numMap))
		acc[char] = [+num, +den];
	return acc;
}, {} as Record<string, [number, number]>);

const revSuperscriptMap = buildReverseMap(superscriptMap);
const revSubscriptMap	= buildReverseMap(subscriptMap);

function transformString(input: string, map: Record<string, string>): string {
	return Array.from(input).map(ch => map[ch] ?? ch).join('');
}

export function toSuperscript(input: string):	string { return transformString(input, superscriptMap);}
export function fromSuperscript(input: string): string { return transformString(input, revSuperscriptMap);}
export function toSubscript(input: string):		string { return transformString(input, subscriptMap);}
export function fromSubscript(input: string): 	string { return transformString(input, revSubscriptMap);}

export function fractionString(num: number, den: number, chars = fractionChars, superSub = true): string {
	return den === 1 ? num.toString()
		: (chars && chars[den]?.[num])
		|| (superSub ? toSuperscript(num.toString()) + '⁄' + toSubscript(den.toString())
			: `${num}⁄${den}`
		);
}

function radicalString(n: number, symbol: string, opts?: FractionOptions): string|undefined {
	if (opts === false)
		return isAlmostInteger(n) ? Math.round(n).toString() : undefined;
	const [num, den] = Num.rationalApprox(n, 1000, 1e-8);
	if (Math.abs(n - num / den) < 1e-10)
		return (n < 0 ? '-' : '') + symbol + fractionString(num, den, opts?.chars, opts?.superSub);
}


export function outputNumber(n: number, opts?: ConstOptions): string {
	if (!Number.isInteger(n)) {
		const f = radicalString(n, '', opts?.fractions);
		if (f)
			return f;
		for (const [i, r] of Object.entries(opts?.radicals ?? radicalChars)) {
			const rf = radicalString(n ** +i, r, opts?.fractions);
			if (rf)
				return rf;
		}
	}
	return n.toString();
}

const reRational	= /^((\d+|[⁰¹²³⁴⁵⁶⁷⁸⁹]+)[/\u2044\u2215](\d+|[₀₁₂₃₄₅₆₇₈₉]+))/;
const reNumber 		= /^(((\d+)[/\u2044\u2215](\d+|[₀₁₂₃₄₅₆₇₈₉]+))|((\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?))/;

export function parseNumber(s: string): [number, algebraic] {
	const c = s.charAt(0);

	for (const [i, r] of Object.entries(radicalChars)) {
		if (c === r) {
			const [offset, num] = parseNumber(s.slice(r.length));
			return [offset + r.length, algebraic.pow(num, rational(1, +i))];
		}
	}

	if (revFractionChars[c]) {
		const [num, den] = revFractionChars[c];
		return [1, num / den];
	}

	const f = reRational.exec(s);
	if (f)
		return [f[0].length, rational(+fromSuperscript(f[2]), +fromSubscript(f[3]))];


	const map = revSuperscriptMap[c] ? revSuperscriptMap : revSubscriptMap[c] ? revSubscriptMap : null;
	if (map) {
		const out: string[] = [];
		for (const ch of s) {
			const m = map[ch];
			if (!m)
				break;
			out.push(m);
		}
		s = out.join('');
	}

	const m = reNumber.exec(s);
	if (m)
		return [m[0].length, m[3] ? rational(+m[3], +fromSubscript(m[4])) : parseFloat(m[0])];

	return [0,0];
}

function parseNumber1(s: string): number {
	const m = reNumber.exec(s);
	if (m)
		return m[3] ? +m[3] / +fromSubscript(m[4]) : parseFloat(m[0]);
	return NaN;
}

type VerticalStyle = {left: string; right: string, mid: number};
export const verticalStyles = {
	bigBraces: {
		left: '⎛⎜⎝', right: '⎞⎟⎠', mid: 1
	},
	bigBraces1: {
		left: ' ⎛⎜ ⎝', right: ' ⎞⎟ ⎠', mid: 2
	},
	brackets: {
		left: ' ⎡⎢⎣', right: ' ⎤⎥⎦', mid: 1
	},
	medBraces: {
		left: '⎧\u23aa⎩', right: '⎫\u23aa⎭', mid: 1
	},
	box: {
		left: '┌│└', right: '┐│┘', mid: 1
	},
};

export function verticalArray(array: string[], style: VerticalStyle): string {
	const { left, right, mid } = style;
	const n = array.length;

	return array.map((line, r) => {
		const i = r < mid ? r : Math.max(r + left.length - n, mid);
		return left[i] + line + right[i];
	}).join('\n');

}

//-----------------------------------------------------------------------------
// parser
//-----------------------------------------------------------------------------

const reSuper 		= /^[⁰¹²³⁴⁵⁶⁷⁸⁹¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᐟ˙]+/;
const reIdentifier	= /^[\p{L}_][\p{L}\d_]*/u;
const reMultiply	= /^[*.⋅×]/;
const reDivide		= /^[/÷]/;

const knownSymbols: Record<string, string> = {
	'π': 'pi',
	'𝑒': 'e',
	'𝑖': 'i',
	'∞': 'infinity',
};

export function parse<T>(ops: Operators<T>, s: string): T {
	let pos = 0;

	function remainder() {
		return s.slice(pos);
	}
	function move(n: number) {
		pos = Math.min(pos + n, s.length);
	}
	function peek(c: string) {
		return s.slice(pos, pos + c.length) === c;
	}
	function skip(c: string) {
		if (remainder().startsWith(c)) {
			pos += c.length;
			return true;
		}
		return false;
	}
	function expect(c: string) {
		if (!skip(c)) 
			throw new Error(`Expected '${c}'`);
	}
	function peekre(re: RegExp) {
		return re.exec(remainder());
	}
	function match(re: RegExp) {
		const m = re.exec(remainder());
		if (m) {
			pos += m.index + m[0].length;
			return m[0];
		}
	}

	function skipSpaces() {
		match(/\s*/);
	}

	function parsePrimary(): T {
		skipSpaces();

		// number
		const [offset, num] = parseNumber(remainder());
		if (offset > 0) {
			move(offset);
			return ops.from(Number(num));
		}

		// parenthesized
		if (skip('(')) {
			const expr = parseExpression();
			skipSpaces();
			expect(')');
			return expr;
		}

		// identifier or function
		let id = match(reIdentifier);
		if (id) {
			// function call
			if (skip('(')) {
				const savepos = pos - 1;
				skipSpaces();
				const args: T[] = [];
				if (!skip(')')) {
					for (;;) {
						args.push(parseExpression());
						skipSpaces();
						if (!skip(','))
							break;
					}
					expect(')');
				}
				const func = ops.func(id, args);
				if (func !== undefined)
					return func;

				if (args.length < 2) {
					const v = ops.variable(id);
					if (v !== undefined) {
						if (args.length === 1)
							pos = savepos;
						return v;
					}
				}
				throw new Error(`Unknown function: ${id}`);
			}

			if (id in knownSymbols)
				id = knownSymbols[id];

			const v = ops.variable(id);
			if (v !== undefined)
				return v;

			throw new Error(`Unknown identifier: ${id}`);
		}

		throw new Error(`Unexpected token at position ${pos}: '${remainder()}'`);
	}

	// parse power (right-associative)
	function parsePower(): T {
		let left = parsePrimary();
		skipSpaces();
		const sup = match(reSuper);
		if (sup) {
			const t = fromSuperscript(sup[0]);
			left = ops.pow(left, ops.from(parseNumber1(t)));
		}
		while (skipSpaces(), skip('^'))
			left = ops.pow(left, parsePower());
		return left;
	}

	// parse multiplication (inc. implicit) and division
	function parseMulDiv(): T {
		let left = parsePower();
		for (;;) {
			skipSpaces();
			if (match(reMultiply) || peekre(/^[\d.]/) || peekre(/^[\p{L}_]/u)) {
				left = ops.mul(left, parsePower());
			} else if (match(reDivide)) {
				left = ops.div(left, parsePower());
			} else if (peek('(')) {
				left = ops.mul(left, parsePower());
			} else {
				break;
			}
		}
		return left;
	}

	// parse unary + and -
	function parseUnary(): T {
		skipSpaces();
		if (skip('+'))
			return parseUnary();
		if (skip('-'))
			return ops.neg(parseUnary());
		return parseMulDiv();
	}

	// parse binary + and -
	function parseExpression(): T {
		let left = parseUnary();
		let op;
		while ((op = (skipSpaces(), match(/^[+-]/)))) {
			const right = parseUnary();
			left = op === '+' ? ops.add(left, right) : ops.sub(left, right);
		}
		return left;
	}

	return parseExpression();
}