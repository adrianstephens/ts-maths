# @isopodlabs/maths

A comprehensive TypeScript mathematics library providing advanced primitives and algorithms for vectors, matrices, rationals, complex numbers, quaternions, polynomials, geometry, and statistics.

## Features

- **Vectors & Matrices**: 2D, 3D, 4D, and higher; swizzling, matrix operations, eigenvalues, LU decomposition.
- **Rational Numbers**: Arbitrary precision, continued fractions, conversion, arithmetic.
- **Complex Numbers**: Arithmetic, transcendental functions, polar form, generics.
- **Quaternions**: Arithmetic, conjugation, normalization, matrix conversion, 3D transformations.
- **Polynomials**: Construction, evaluation, derivatives, root finding (real/complex), Sturm/Aberth methods.
- **Geometry**: Planes, colinearity, curve operations, root refinement, intersection.
- **Statistics**: Mean, variance, standard deviation for scalars and vectors.
- **Symbolic Algebra**: (see `symbolic.ts`) Expression graphs, rule-based simplification.
- **Extensible**: Most types are generic over custom scalar types.

## Installation

```sh
npm install @isopodlabs/maths
```

## Usage Examples

### Vectors

```typescript
import { float2, float3 } from '@isopodlabs/maths';

const a = float2(1, 2);
const b = float3(3, 4, 5);

const sum = a.add(float2(1, 1));     // (2, 3)
const dot = a.dot(float2(2, 3));     // 8
const len = a.len();                 // 2.236...
const swiz = b.xy;                   // float2(3, 4)
```

### Complex Numbers

```typescript
import { complex } from '@isopodlabs/maths';

const z1 = complex(3, 4);           // 3 + 4i
const z2 = complex(1, -1);          // 1 - i

const sum = z1.add(z2);             // 4 + 3i
const product = z1.mul(z2);         // 7 + i
const magnitude = z1.abs();         // 5
const phase = z1.arg();             // 0.927...
const exp_z = complex.exp(z1);
const sin_z = complex.sin(z1);
```

### Polynomials

```typescript
import { Polynomial } from '@isopodlabs/maths';

const p = new Polynomial([1, 0, -2]); // x^2 - 2
const roots = p.realRoots();          // [sqrt(2), -sqrt(2)]
const value = p.evaluate(3);          // 7
const deriv = p.deriv();              // 2x
```

### Quaternions

```typescript
import { quaternion, float3 } from '@isopodlabs/maths';

const q = quaternion.fromAxisAngle(float3(0, 0, 1), Math.PI / 2);
const v = float3(1, 0, 0);
const rotated = q.transform(v); // Rotates v by 90° around z
```

### Geometry

```typescript
import { plane2, float2 } from '@isopodlabs/maths';

const p0 = float2(0, 0), p1 = float2(1, 1);
const plane = plane2.fromVerts(p0, p1);
const dist = plane.dist(float2(2, 2));
```

### Statistics

```typescript
import { statistics1 } from '@isopodlabs/maths';

const stats = statistics1.from([1, 2, 3, 4, 5]);
console.log(stats.mean, stats.variance, stats.standardDeviation);
```

## API Reference

See source files in `src/` for full type definitions and advanced features:
- `vector.ts`, `vector2.ts`: Vectors, matrices, swizzling, linear algebra
- `complex.ts`: Complex numbers, transcendental functions
- `quaternion.ts`: Quaternions, 3D rotation
- `polynomial.ts`: Polynomials, root finding
- `geometry.ts`: Planes, curves, intersection
- `rational.ts`: Rational numbers, continued fractions
- `statistics.ts`: Scalar and vector statistics
- `symbolic.ts`, `egraph.ts`: Symbolic algebra, expression graphs

## Contributing

Follow the monorepo's [Copilot Instructions](../.github/copilot-instructions.md) for code style and architecture guidelines.

---
