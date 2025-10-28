# @isopodlabs/maths
[![npm version](https://img.shields.io/npm/v/@isopodlabs/maths.svg)](https://www.npmjs.com/package/@isopodlabs/maths)
[![GitHub stars](https://img.shields.io/github/stars/adrianstephens/ts-maths.svg?style=social)](https://github.com/adrianstephens/ts-maths)
[![License](https://img.shields.io/npm/l/@isopodlabs/maths.svg)](LICENSE.txt)

A comprehensive TypeScript mathematics library providing vector operations, rationals, complex numbers, quaternions, polynomial solving, and geometric primitives.

## ☕ Support My Work
If you use this package, consider [buying me a cup of tea](https://coff.ee/adrianstephens) to support future updates!

## Installation

```sh
npm install @isopodlabs/maths
```

## Features

### Vector Math
- **2D, 3D, 4D vectors** with named properties (`x`, `y`, `z`, `w`)
- **Swizzling support** - access `vec.xy`, `vec.xyz`, etc.
- **Matrix operations** - 2x2, 2x3, 3x3, 3x4, 4x4 matrices
- **Utility functions** - normalize, lerp, project, reflect
- **Bounding boxes** - extent1, extent2, extent3
- **Statistics** - mean, variance for scalars and vectors

### Rational Numbers
- **Basic operations** - add, multiply, divide, negate
- **BigInt** - rationals over bigint
- **Generics** - rationals over custom types that support arithmetic through add, mul, etc, methods

### Complex Numbers
- **Basic operations** - add, multiply, divide, negate, conjugate
- **Transcendental functions** - sin, cos, exp, log, sqrt
- **Polar form** - magnitude, argument, from polar coordinates
- **Generics** - complex numbers over custom types that support arithmetic through add, mul, etc, methods

### Quaternions
- **Basic operations** - add, multiply, divide, negate, conjugate
- **Transcendental functions** - exp, log
- #### Subclass for unit quaternions ####
  - **Conversion** to/from 3x3 matrix, axis-angle, euler angles

### Polynomials
- **Root finding** - real and complex
- **Evaluation** - single values, arrays, complex numbers
- **Operations** - add, subtract, multiply, derivative
- **Advanced solving** - high-degree polynomials use Aberth(complex) or Sturm(real) root finding methods
- **Generics** - polynomials over custom types that support arithmetic through add, mul, etc, methods

### Geometry
- **Shapes** - lines, circles, Bézier curves (quadratic/cubic)
- **Splines** - connected sequences of curves
- **Utilities** - arc length, closest points, intersections
- **Curve reduction** - convert cubic to quadratic Béziers

## Usage

### Vectors

```typescript
import { float2, float3 } from '@isopodlabs/maths';

// Create vectors
const a = float2(1, 2);
const b = float3(3, 4, 5);

// Vector operations
const sum = a.add(float2(1, 1));     // (2, 3)
const dot = a.dot(float2(2, 3));     // 8
const len = a.len();                 // 2.236...

// Swizzling
const swiz = b.xy;                   // float2(3, 4)
const rev = b.zyx;                   // float3(5, 4, 3)

// Matrix operations
const m = float2x2(float2(1, 0), float2(0, 1));
const transformed = mul2x2(m, a);
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

// Transcendental functions
const exp_z = complex.exp(z1);
const sin_z = complex.sin(z1);
```

### Polynomials

```typescript
import { polynomial } from '@isopodlabs/maths';

// Create polynomial: x² - 5x + 6 = (x-2)(x-3)
const poly = new polynomial([6, -5, 1]);

const roots = poly.roots();         // [2, 3]
const value = poly.evaluate(2);     // 0
const deriv = poly.deriv();         // -5 + 2x

// Complex roots
const allRoots = poly.allRoots();   // includes complex solutions
```

### Geometry

```typescript
import { line2, bezier2, circle } from '@isopodlabs/maths';

// Line from (0,0) to (1,1)
const line = new line2(float2(0, 0), float2(1, 1));
const midpoint = line.evaluate(0.5); // (0.5, 0.5)

// Quadratic Bézier curve
const curve = new bezier2(
    float2(0, 0),    // start
    float2(1, 1),    // control
    float2(2, 0)     // end
);
const point = curve.evaluate(0.5);
const length = curve.lengthTo(1);

// Circle
const circ = new circle(float2(0, 0), 1);
const circumference = circ.lengthTo(1); // 2π
```

## API Reference

### Core Types
- `float2`, `float3`, `float4` - Vector types with swizzling
- `float2x2`, `float2x3`, `float3x3`, etc. - Matrix types
- `complex` - Complex number type
- `polynomial`, `polynomialN` - Polynomial types

### Utility Functions
- `normalise(v)` - Normalize vector to unit length
- `lerp(a, b, t)` - Linear interpolation
- `project(a, b)` - Project vector a onto b
- `mid(a, b)` - Midpoint between vectors

### Geometric Shapes
- `line<T>` - Line segment
- `circle` - Circle with center and radius
- `bezier2<T>`, `bezier3<T>` - Bézier curves
- `polygon<T>` - Closed polygon

## Performance Notes

- Vectors use object properties (`{x, y, z}`) for optimal performance in V8
- All operations return new instances (immutable)
- Swizzling is implemented with getters for zero-copy access
- Matrix operations are optimized for small fixed sizes

## License

This project is licensed under the MIT License.