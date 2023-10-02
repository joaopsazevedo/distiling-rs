use crate::Strategy::{Max, Simple};
use anyhow::{anyhow, bail};
use std::collections::HashMap;
use std::env;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Coordinate {
    l: usize,
    c: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Strategy {
    Simple,
    GreatThan(usize),
    Max,
}

impl Display for Strategy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Simple => write!(f, "-1"),
            Strategy::GreatThan(v) => write!(f, "{}", v),
            Max => write!(f, "-3"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Problem {
    l: usize,
    c: usize,
    v: Strategy,
    deleted_blocks: Vec<Coordinate>,
    score: usize,
    tiles: Vec<isize>,
    tiles_kind_counter: Vec<usize>,
}

impl Display for Problem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for l in 0..self.l {
            for c in 0..self.c {
                write!(f, "{} ", self.tiles[l * self.c + c])?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

// impl PartialOrd for Problem {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(match self.score.cmp(&other.score) {
//             std::cmp::Ordering::Less => std::cmp::Ordering::Less,
//             std::cmp::Ordering::Equal => self.score.cmp(&other.score),
//             std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
//         })
//     }
// }

// impl Ord for Problem {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         match self.deleted_blocks.len().cmp(&other.deleted_blocks.len()) {
//             std::cmp::Ordering::Less => std::cmp::Ordering::Less,
//             std::cmp::Ordering::Equal => self.score.cmp(&other.score),
//             std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
//         }
//     }
// }

impl Problem {
    fn new<I>(numbers: &mut I) -> Result<Self, anyhow::Error>
    where
        I: Iterator<Item = isize>,
    {
        let l = numbers.next().ok_or(anyhow!("Failed to get L"))?;
        let c = numbers.next().ok_or(anyhow!("Failed to get C"))?;
        let v = numbers.next().ok_or(anyhow!("Failed to get v"))?;
        if l <= 0 {
            bail!("Expected a positive L ({})", l)
        }
        if c <= 0 {
            bail!("Expected a positive C ({})", c)
        }
        let l = l as usize;
        let c = c as usize;
        let v = match v {
            -3 => Max,
            -1 => Simple,
            0..=isize::MAX => Strategy::GreatThan(v as usize),
            _ => bail!("Expected a v ({}) equal to -3, -1 or positive", v),
        };
        let tiles = numbers.take(c * l).collect::<Vec<_>>();
        if tiles.len() < c * l {
            bail!("Invalid problem definition")
        }
        let mut tiles_kind_counter = vec![0; *tiles.iter().max().unwrap_or(&0) as usize];
        tiles.iter().for_each(|tile| {
            tiles_kind_counter[*tile as usize - 1] += 1;
        });
        Ok(Self {
            l,
            c,
            v,
            deleted_blocks: vec![],
            tiles,
            score: 0,
            tiles_kind_counter,
        })
    }

    fn visited_at<'a>(
        visited: &'a Vec<bool>,
        line_length: usize,
        coordinate: &Coordinate,
    ) -> Result<&'a bool, anyhow::Error> {
        visited
            .get(coordinate.l * line_length + coordinate.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", coordinate))
    }

    fn visited_at_mut<'a>(
        visited: &'a mut Vec<bool>,
        line_length: usize,
        coordinate: &Coordinate,
    ) -> Result<&'a mut bool, anyhow::Error> {
        visited
            .get_mut(coordinate.l * line_length + coordinate.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", coordinate))
    }

    fn reset_visited(visited: &mut Vec<bool>) {
        visited.iter_mut().for_each(|v| *v = false);
    }

    fn at(&self, c: &Coordinate) -> Result<&isize, anyhow::Error> {
        self.tiles
            .get(c.l * self.c + c.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", c))
    }

    fn at_mut(&mut self, c: &Coordinate) -> Result<&mut isize, anyhow::Error> {
        self.tiles
            .get_mut(c.l * self.c + c.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", c))
    }

    fn neighbours(&self, c: &Coordinate) -> Vec<Coordinate> {
        vec![
            Coordinate {
                l: c.l,
                c: c.c.saturating_sub(1),
            },
            Coordinate { l: c.l, c: c.c + 1 },
            Coordinate {
                l: c.l.saturating_sub(1),
                c: c.c,
            },
            Coordinate { l: c.l + 1, c: c.c },
        ]
        .into_iter()
        .filter(|n| *n != *c && n.c < self.c && n.l < self.l)
        .collect()
    }

    fn set_as_visited(
        visited: &mut Vec<bool>,
        line_length: usize,
        coordinate: &Coordinate,
    ) -> Result<(), anyhow::Error> {
        *Self::visited_at_mut(visited, line_length, coordinate)? = true;
        Ok(())
    }

    fn delete_tile(&mut self, c: &Coordinate) -> Result<(), anyhow::Error> {
        Ok(*self.at_mut(c)? = -1)
    }

    fn step(
        &mut self,
        coordinate: &Coordinate,
        visited: &mut Vec<bool>,
    ) -> Result<usize, anyhow::Error> {
        self.delete(coordinate, visited).and_then(|deleted| {
            if deleted > 1 {
                self.vertical_slide()?;
                self.horizontal_slide()?;
            }
            Ok(deleted)
        })
    }

    fn delete(
        &mut self,
        coordinate: &Coordinate,
        visited: &mut Vec<bool>,
    ) -> Result<usize, anyhow::Error> {
        Self::reset_visited(visited);
        let target = *self.at(coordinate)?;
        self.delete_rec(&coordinate, target, visited)
    }

    fn delete_rec(
        &mut self,
        coordinate: &Coordinate,
        target: isize,
        visited: &mut Vec<bool>,
    ) -> Result<usize, anyhow::Error> {
        let line_length = self.c;
        Self::set_as_visited(visited, self.c, coordinate)?;
        self.delete_tile(coordinate)?;
        self.neighbours(coordinate)
            .iter()
            .try_fold(1, |acc, neighbour_coordinate| {
                let neighbour_value = *self.at(neighbour_coordinate)?;
                if neighbour_value == target
                    && !*Self::visited_at(&visited, line_length, neighbour_coordinate)?
                {
                    Ok(acc + self.delete_rec(neighbour_coordinate, target, visited)?)
                } else {
                    Ok(acc)
                }
            })
    }

    fn vertical_slide(&mut self) -> Result<(), anyhow::Error> {
        for c in 0..self.c {
            for l in (1..self.l).rev() {
                let current_tile_coordinate = Coordinate { l, c };
                let current_tile_value = *self.at(&current_tile_coordinate)?;
                if current_tile_value == -1 {
                    for l_top in (0..=l - 1).rev() {
                        let top_tile_coordinate = Coordinate { l: l_top, c };
                        let top_tile_value = *self.at(&top_tile_coordinate)?;
                        if top_tile_value != -1 {
                            *self.at_mut(&current_tile_coordinate)? = top_tile_value;
                            *self.at_mut(&top_tile_coordinate)? = -1;
                            break;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn horizontal_slide(&mut self) -> Result<(), anyhow::Error> {
        for c in (1..self.c).rev() {
            let bottom_tile_coordinate = Coordinate { l: self.l - 1, c };
            let bottom_tile_value = *self.at(&bottom_tile_coordinate)?;
            if bottom_tile_value == -1 {
                for i in (0..=c - 1).rev() {
                    let left_tile_coordinate = Coordinate {
                        l: self.l - 1,
                        c: i,
                    };
                    let left_tile_value = *self.at(&left_tile_coordinate)?;
                    if left_tile_value != -1 {
                        for l in 0..self.l {
                            let left_coordinate = Coordinate { l, c: i };
                            let right_coordinate = Coordinate { l, c };
                            let left_value = *self.at(&left_coordinate)?;
                            *self.at_mut(&right_coordinate)? = left_value;
                            *self.at_mut(&left_coordinate)? = -1;
                        }
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    fn get_deletable_block(&self) -> Option<Coordinate> {
        for l in (0..self.l).rev() {
            for c in 0..self.c {
                let tile = Coordinate { l, c };
                let tile_value = *self.at(&tile).unwrap();
                if tile_value != -1
                    && self
                        .neighbours(&tile)
                        .into_iter()
                        .any(|n| *self.at(&n).unwrap() == tile_value)
                {
                    return Some(tile);
                }
            }
        }
        None
    }

    fn get_all_deletable_blocks(
        &mut self,
        visited: &mut Vec<bool>,
    ) -> Result<Vec<Coordinate>, anyhow::Error> {
        Self::reset_visited(visited);
        let mut deletable_blocks = vec![];
        for l in (0..self.l).rev() {
            for c in 0..self.c {
                let tile = Coordinate { l, c };
                if !*Self::visited_at(visited, self.c, &tile)? {
                    Self::set_as_visited(visited, self.c, &tile)?;
                    let tile_value = *self.at(&tile)?;
                    if tile_value == -1 {
                        continue;
                    }
                    let mut block_size = 1;
                    let mut neighbours_to_explore = self.neighbours(&tile);
                    while let Some(neighbour) = neighbours_to_explore.pop() {
                        if !*Self::visited_at(visited, self.c, &neighbour)?
                            && *self.at(&neighbour)? == tile_value
                        {
                            block_size += 1;
                            Self::set_as_visited(visited, self.c, &neighbour)?;
                            neighbours_to_explore.extend(self.neighbours(&neighbour));
                        }
                    }
                    if block_size != 1 {
                        deletable_blocks.push((tile, block_size));
                    }
                }
            }
        }
        // deletable_blocks.sort_by(|l, r| l.1.cmp(&r.1));
        Ok(deletable_blocks
            .into_iter()
            .map(|(block_tile, _)| block_tile)
            .collect())
    }

    fn solve(&mut self) -> Result<(), anyhow::Error> {
        match self.v {
            Simple => self.solve_simple(),
            Strategy::GreatThan(v) => self.solve_greater_than(v),
            Max => self.solve_max(),
        }
    }

    fn solve_simple(&mut self) -> Result<(), anyhow::Error> {
        let mut visited = vec![false; self.tiles.len()];
        while let Some(deletable_block) = self.get_deletable_block() {
            let deleted = self.step(&deletable_block, &mut visited)?;
            self.score += deleted * (deleted - 1);
            self.deleted_blocks.push(deletable_block);
        }
        println!("{} {} {}", self.l, self.c, self.v);
        println!("{} {}", self.deleted_blocks.len(), self.score);
        self.deleted_blocks
            .iter()
            .for_each(|deleted| println!("{} {}", self.l - deleted.l, deleted.c + 1));
        println!();
        Ok(())
    }

    fn solve_greater_than(&mut self, v: usize) -> Result<(), anyhow::Error> {
        let mut visited = vec![false; self.l * self.c];
        let mut best_so_far: Option<(usize, usize)> = None;
        let mut problem_stack = vec![self.clone()];
        while let Some(mut problem) = problem_stack.pop() {
            if problem.score >= v {
                println!("{} {} {}", problem.l, problem.c, problem.v);
                println!("{} {}", problem.deleted_blocks.len(), problem.score);
                problem.deleted_blocks.iter().for_each(|deleted| {
                    println!("{} {}", problem.l - deleted.l, deleted.c + 1);
                });
                println!();
                return Ok(());
            }
            if let Some((least_steps, _)) = best_so_far {
                if least_steps < problem.deleted_blocks.len() {
                    continue;
                }
            }
            let all_deletable_blocks = problem.get_all_deletable_blocks(&mut visited)?;
            if all_deletable_blocks.is_empty() {
                if let Some((least_steps, best_score)) = best_so_far {
                    match least_steps.cmp(&problem.deleted_blocks.len()) {
                        std::cmp::Ordering::Equal => {
                            if best_score > problem.score {
                                best_so_far = Some((problem.deleted_blocks.len(), problem.score));
                            }
                        }
                        _ => {}
                    }
                } else {
                    best_so_far = Some((problem.deleted_blocks.len(), problem.score));
                }
            } else {
                let next_problems = all_deletable_blocks
                    .into_iter()
                    .rev()
                    .map(|deletable_block| -> Result<Problem, anyhow::Error> {
                        let mut new_problem = problem.clone();
                        let deleted = new_problem.step(&deletable_block, &mut visited)?;
                        new_problem.deleted_blocks.push(deletable_block);
                        new_problem.score += deleted * (deleted - 1);
                        Ok(new_problem)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                problem_stack.extend(next_problems);
            }
        }
        println!("{} {} {}", self.l, self.c, self.v);
        println!("0 -1");
        println!();
        Ok(())
    }

    fn best_possible_score(&self) -> usize {
        self.tiles_kind_counter.iter().fold(self.score, |acc, n| {
            if *n == 0 {
                acc
            } else {
                acc + n * (n - 1)
            }
        })
    }

    fn solve_max_dfs_cache(
        &mut self,
        cache: &mut HashMap<(Vec<isize>, usize), Option<Self>>,
        best: &mut Self,
    ) -> Result<Option<Self>, anyhow::Error> {
        if let Some(best_solution) = cache.get(&(self.tiles.clone(), self.score)) {
            return Ok(best_solution.clone());
        }

        if best.deleted_blocks.len() > 0 && self.best_possible_score() <= best.score {
            cache.insert((self.tiles.clone(), self.score), None);
            return Ok(None);
        }

        let mut visited = vec![false; self.tiles.len()];
        let deletable_blocks = self.get_all_deletable_blocks(&mut visited)?;
        if deletable_blocks.is_empty() {
            if best.deleted_blocks.len() == 0 {
                *best = self.clone();
                cache.insert((self.tiles.clone(), self.score), Some(self.clone()));
                return Ok(Some(self.clone()));
            } else {
                match self.score.cmp(&best.score) {
                    std::cmp::Ordering::Greater => {
                        *best = self.clone();
                        cache.insert((self.tiles.clone(), self.score), Some(self.clone()));
                        return Ok(Some(self.clone()));
                    }
                    _ => {}
                }
            }
            cache.insert((self.tiles.clone(), self.score), None);
            return Ok(None);
        }

        let best_solution = deletable_blocks
            .into_iter()
            .map(
                |deletable_block| -> Result<Option<Problem>, anyhow::Error> {
                    let mut new_problem = self.clone();
                    let tile_value = new_problem.at(&deletable_block)?.clone();
                    let deleted = new_problem.step(&deletable_block, &mut visited)?;
                    new_problem.tiles_kind_counter[tile_value as usize - 1] -= deleted;
                    new_problem.deleted_blocks.push(deletable_block);
                    new_problem.score += deleted * (deleted - 1);
                    new_problem.solve_max_dfs_cache(cache, best)
                },
            )
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|p| p)
            .into_iter()
            .max_by(|l, r| l.score.cmp(&r.score));
        cache.insert((self.tiles.clone(), self.score), best_solution.clone());
        Ok(best_solution)
    }

    fn solve_max(&mut self) -> Result<(), anyhow::Error> {
        let mut cache = HashMap::new();
        let mut best = self.clone();
        let best = self
            .solve_max_dfs_cache(&mut cache, &mut best)?
            .ok_or(anyhow!("solution not found"))?;

        println!("{} {} {}", best.l, best.c, best.v);
        println!("{} {}", best.deleted_blocks.len(), best.score);
        best.deleted_blocks.iter().for_each(|deleted| {
            println!("{} {}", best.l - deleted.l, deleted.c + 1);
        });
        println!();
        Ok(())
    }
}

fn main() -> Result<(), anyhow::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        bail!("Invalid arguments");
    }

    let input_filename = args.get(1).ok_or(anyhow!("Invalid arguments"))?;
    let mut file = File::open(input_filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let numbers = contents
        .split_whitespace()
        .map(|s| s.parse::<isize>())
        .collect::<Result<Vec<_>, _>>()?;
    let mut numbers_iter = numbers.into_iter().peekable();

    while numbers_iter.peek().is_some() {
        let mut problem = Problem::new(&mut numbers_iter)?;
        problem.solve()?;
    }

    Ok(())
}
