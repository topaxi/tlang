struct Vector {
    x: int,
    y: int,
}

// Vector::new(int, int) -> Vector
fn Vector::new(x: int, y: int) -> Vector {
    Vector { x, y }
}

// Vector.add(self, Vector) -> Vector
fn Vector.add(self, other: Vector) -> Vector {
    Vector::new(self.x + other.x, self.y + other.y)
}

// Vector.scale(self, int) -> Vector
fn Vector.scale(self, scalar: int) -> Vector {
    Vector::new(self.x * scalar, self.y * scalar)
}

// Vector.magnitude(self) -> float
fn Vector.magnitude(self) -> float {
    math::sqrt(self.x ** 2 + self.y ** 2)
}

// Example usage
let v1 = Vector::new(3, 4);

// Vector addition
v1.add(Vector::new(1, 2))
|> log();

// Scaling a vector
v1.scale(2)
|> log();

// Magnitude of a vector
v1.magnitude()
|> log();
