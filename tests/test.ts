export interface Equal<T> {
	equal(b: T): boolean;
}
type Testable<T> = T extends (string | number | boolean | null | undefined) 
	? T 
	: T & Equal<T>;

export function expect<T>(v: Testable<T>, description?: string) {
	return {
		toEqual(v2: T) {
			const success = typeof v === 'object' && v ? v.equal(v2) : v === v2;
			console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}${v} === ${v2}`);
			//if (!success)
			//	console.log(`fail: expected ${v2}, got ${v}`);
		},
		check(test: (v: T) => boolean) {
			const success = test(v);
			console.log(`${success ? '✓' : '✗'}: ${description ?? ''}`);
		}
	};
}

export function test(name: string, fn: ()=>void) {
	console.log('---------------------');
	console.log("testing: " + name);
	fn();
	console.log("finished: " + name);
}

export function assert(condition: boolean, msg?: string) {
	if (!condition)
		throw new Error(msg || 'assertion failed');
}
