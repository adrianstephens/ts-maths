import { Operators } from './core';
import Num, { isAlmostInteger } from './num';

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

type VerticalStyle = {left: string; right: string, mid: number};
export const verticalStyles = {
	bigBraces: {
		left: '⎛⎜⎝', right: '⎞⎟⎠', mid: 1		//macos
	},
	bigBraces1: {
		left: ' ⎛⎜ ⎝', right: ' ⎞⎟ ⎠', mid: 2	//windows
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

const reSuper 		= /^[⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᐟ˙]+/;
const reIdentifier	= /^[\p{L}_][\p{L}\d_]*/u;
const reMultiply	= /^[*.⋅×]/;
const reDivide		= /^[/÷]/;
const reRational	= /^((\d+|[⁰¹²³⁴⁵⁶⁷⁸⁹]+)[/\u2044\u2215](\d+|[₀₁₂₃₄₅₆₇₈₉]+))/;
const reNumber 		= /^(((\d+)[/\u2044\u2215](\d+|[₀₁₂₃₄₅₆₇₈₉]+))|((\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?))/;
const revRadical	= buildReverseMap(radicalChars);

const knownSymbols: Record<string, string> = {
	'π': 'pi',
	'𝑒': 'e',
	'𝑖': 'i',
	'∞': 'infinity',
};

export function parse<T>(ops: Operators<T>, s: string): T {
	let pos = 0;

	function remainder() 			{ return s.slice(pos); }
	function move(n: number) 		{ pos = Math.min(pos + n, s.length); }
	function peek(n = 1) 			{ return s.slice(pos, pos + n); }
	function check(c: string) 		{ return peek(c.length) === c; }
	function checkre(re: RegExp)	{ return re.exec(remainder()); }

	function skip(c: string) {
		if (!check(c))
			return false;
		pos += c.length;
		return true;
	}
	function skipre(re: RegExp) {
		const m = re.exec(remainder());
		if (m) {
			pos += m.index + m[0].length;
			return m;
		}
	}
	function expect(c: string) {
		if (!skip(c)) 
			throw new Error(`Expected '${c}'`);
	}
	function skipSpaces() {
		skipre(/\s*/);
	}

	function rational(n: number, d: number) {
		if (d === 1)
			return ops.from(n);
		return ops.div(ops.from(n), ops.from(d));
	}

	function number(m: string[]) {
		return m[3] ? rational(+m[3], +fromSubscript(m[4])) : ops.from(parseFloat(m[0]));
	}

	function parsePrimary(): T {
		skipSpaces();

		// number

		let m = skipre(reRational);
		if (m)
			return rational(+fromSuperscript(m[2]), +fromSubscript(m[3]));

		const c = peek();
		const frac = revFractionChars[c];
		if (frac) {
			move(1);
			return rational(frac[0], frac[1]);
		}

		const map = revSuperscriptMap[c] ? revSuperscriptMap : revSubscriptMap[c] ? revSubscriptMap : null;
		if (map) {
			const out: string[] = [];
			for (const ch of s) {
				const m = map[ch];
				if (!m)
					break;
				out.push(m);
			}
			const m = reNumber.exec(out.join(''));
			if (m)
				return number(m);
		}

		m = skipre(reNumber);
		if (m)
			return number(m);

		// parenthesized
		if (skip('(')) {
			const expr = parseExpression();
			skipSpaces();
			expect(')');
			return expr;
		}

		// identifier or function
		m = skipre(reIdentifier);
		if (m) {
			let id = m[0];

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
		const r = revRadical[peek()];
		if (r) {
			move(1);
			return ops.rpow(parsePower(), 1, +r);
		}

		let left = parsePrimary();
		skipSpaces();
		const sup = skipre(reSuper);
		if (sup) {
			const m = reNumber.exec(fromSuperscript(sup[0]));
			if (!m)
				throw new Error(`Invalid superscript: ${sup[0]}`);
			const pow = m[3] ? rational(+m[3], +fromSubscript(m[4])) : ops.from(parseFloat(m[0]));
			left = ops.pow(left, pow);
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
			if (skipre(reMultiply) || checkre(/^[\d.]/) || checkre(/^[\p{L}_]/u)) {
				left = ops.mul(left, parsePower());
			} else if (skipre(reDivide)) {
				left = ops.div(left, parsePower());
			} else if (check('(')) {
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
		while ((op = (skipSpaces(), skipre(/^[+-]/)))) {
			const right = parseUnary();
			left = op[0] === '+' ? ops.add(left, right) : ops.sub(left, right);
		}
		return left;
	}

	return parseExpression();
}