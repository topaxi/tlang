struct Point {
    x: int,
    y: int,
}

fn Point::new(x: int, y: int) -> Point {
    Point {
        x: x,
        y: y,
    }
}

// Point.get_coordinates(self) -> int[]
fn Point.get_coordinates(self) {
    [self.x, self.y]
}

let p = Point::new(5, 10);

p.get_coordinates()
|> log();
