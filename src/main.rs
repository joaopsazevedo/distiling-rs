use crate::Strategy::{Max, Simple};
use anyhow::{anyhow, bail};
use std::collections::{HashMap, HashSet};
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
struct Solution {
    deleted_blocks: Vec<Coordinate>,
    score: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProblemState {
    tiles: Vec<isize>,
    score: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProblemStateHelper {
    tiles_kind_counter: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Problem {
    l: usize,
    c: usize,
    v: Strategy,
    state: ProblemState,
    state_helper: ProblemStateHelper,
}

impl Display for Problem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for l in 0..self.l {
            for c in 0..self.c {
                write!(f, "{} ", self.state.tiles[l * self.c + c])?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

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
        let mut tiles_kind_counter = vec![0; *tiles.iter().max().unwrap_or(&0).max(&0) as usize];
        tiles.iter().for_each(|&tile| {
            if tile > 0 {
                tiles_kind_counter[tile as usize - 1] += 1;
            }
        });
        let state = ProblemState { tiles, score: 0 };
        let state_helper = ProblemStateHelper { tiles_kind_counter };
        Ok(Self {
            l,
            c,
            v,
            state,
            state_helper,
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

    fn at(&self, coordinate: &Coordinate) -> Result<&isize, anyhow::Error> {
        self.state
            .tiles
            .get(coordinate.l * self.c + coordinate.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", coordinate))
    }

    fn at_mut(&mut self, coordinate: &Coordinate) -> Result<&mut isize, anyhow::Error> {
        self.state
            .tiles
            .get_mut(coordinate.l * self.c + coordinate.c)
            .ok_or(anyhow!("Can't get visited at ({:?})", coordinate))
    }

    fn neighbours(&self, coordinate: &Coordinate) -> Vec<Coordinate> {
        vec![
            Coordinate {
                l: coordinate.l,
                c: coordinate.c.saturating_sub(1),
            },
            Coordinate {
                l: coordinate.l,
                c: coordinate.c + 1,
            },
            Coordinate {
                l: coordinate.l.saturating_sub(1),
                c: coordinate.c,
            },
            Coordinate {
                l: coordinate.l + 1,
                c: coordinate.c,
            },
        ]
        .into_iter()
        .filter(|n| *n != *coordinate && n.c < self.c && n.l < self.l)
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

    fn delete_tile(&mut self, coordinate: &Coordinate) -> Result<(), anyhow::Error> {
        Ok(*self.at_mut(coordinate)? = -1)
    }

    fn step(
        &mut self,
        coordinate: &Coordinate,
        visited: &mut Vec<bool>,
    ) -> Result<usize, anyhow::Error> {
        Self::reset_visited(visited);
        let tile_value = self.at(&coordinate)?.clone();
        self.delete(coordinate, visited).and_then(|deleted| {
            let score = deleted * (deleted - 1);
            self.vertical_slide()?;
            self.horizontal_slide()?;
            self.state_helper.tiles_kind_counter[tile_value as usize - 1] -= deleted;
            self.state.score += score;
            Ok(score)
        })
    }

    fn delete(
        &mut self,
        coordinate: &Coordinate,
        visited: &mut Vec<bool>,
    ) -> Result<usize, anyhow::Error> {
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
                        deletable_blocks.push(tile);
                    }
                }
            }
        }
        Ok(deletable_blocks)
    }

    fn best_possible_score(&self) -> usize {
        self.state_helper
            .tiles_kind_counter
            .iter()
            .fold(0, |acc, n| if *n == 0 { acc } else { acc + n * (n - 1) })
    }

    fn solve(&mut self) -> Result<(), anyhow::Error> {
        match self.v {
            Simple => self.solve_simple(),
            Strategy::GreatThan(v) => self.solve_greater_than(v),
            Max => self.solve_max(),
        }
    }

    fn solve_simple(&mut self) -> Result<(), anyhow::Error> {
        let mut visited = vec![false; self.state.tiles.len()];
        let mut solution = Solution {
            deleted_blocks: vec![],
            score: 0,
        };
        while let Some(deletable_block) = self.get_deletable_block() {
            let deleted = self.step(&deletable_block, &mut visited)?;
            solution.score += deleted * (deleted - 1);
            solution.deleted_blocks.push(deletable_block);
        }
        println!("{} {} {}", self.l, self.c, self.v);
        println!("{} {}", solution.deleted_blocks.len(), solution.score);
        solution
            .deleted_blocks
            .iter()
            .for_each(|deleted| println!("{} {}", self.l - deleted.l, deleted.c + 1));
        println!();
        Ok(())
    }

    fn solve_greater_than_dfs(
        &mut self,
        target: usize,
        solution: &mut Solution,
        visited: &mut Vec<bool>,
        cache: &mut HashSet<ProblemState>,
    ) -> Result<Option<()>, anyhow::Error> {
        if cache.contains(&self.state) {
            return Ok(None);
        } else {
            cache.insert(self.state.clone());
        }

        if solution.score + self.best_possible_score() < target {
            return Ok(None);
        }

        for deletable_block in self.get_all_deletable_blocks(visited)? {
            let mut new_problem = self.clone();
            let score = new_problem.step(&deletable_block, visited)?;

            solution.score += score;
            solution.deleted_blocks.push(deletable_block);

            if solution.score >= target {
                return Ok(Some(()));
            }

            if new_problem
                .solve_greater_than_dfs(target, solution, visited, cache)?
                .is_some()
            {
                return Ok(Some(()));
            } else {
                solution.score -= score;
                solution.deleted_blocks.pop();
            }
        }

        Ok(None)
    }

    fn solve_greater_than(&mut self, v: usize) -> Result<(), anyhow::Error> {
        let mut solution = Solution {
            score: 0,
            deleted_blocks: vec![],
        };
        let mut visited = vec![false; self.state.tiles.len()];
        let mut cache = HashSet::new();
        match self.solve_greater_than_dfs(v, &mut solution, &mut visited, &mut cache)? {
            Some(_) => {
                println!("{} {} {}", self.l, self.c, self.v);
                println!("{} {}", solution.deleted_blocks.len(), solution.score);
                solution.deleted_blocks.iter().for_each(|deleted| {
                    println!("{} {}", self.l - deleted.l, deleted.c + 1);
                });
            }
            None => {
                println!("{} {} {}", self.l, self.c, self.v);
                println!("0 -1");
            }
        }
        println!();
        Ok(())
    }

    fn solve_max_dfs(
        &mut self,
        visited: &mut Vec<bool>,
        cache: &mut HashMap<ProblemState, Option<Solution>>,
        best_score_so_far: &mut usize,
    ) -> Result<Option<Solution>, anyhow::Error> {
        if let Some(solution) = cache.get(&self.state) {
            return Ok(solution.clone());
        }

        let deletable_blocks = self.get_all_deletable_blocks(visited)?;
        if deletable_blocks.is_empty() {
            *best_score_so_far = (*best_score_so_far).max(self.state.score);
            return Ok(Some(Solution {
                deleted_blocks: vec![],
                score: 0,
            }));
        }

        let best_solution = deletable_blocks
            .into_iter()
            .map(
                |deletable_block| -> Result<Option<Solution>, anyhow::Error> {
                    let mut new_problem = self.clone();
                    let score = new_problem.step(&deletable_block, visited)?;

                    if new_problem.state.score + new_problem.best_possible_score()
                        <= *best_score_so_far
                    {
                        return Ok(None);
                    }

                    if let Some(mut new_problem_solution) =
                        new_problem.solve_max_dfs(visited, cache, best_score_so_far)?
                    {
                        new_problem_solution.score += score;
                        new_problem_solution.deleted_blocks.push(deletable_block);
                        Ok(Some(new_problem_solution))
                    } else {
                        Ok(None)
                    }
                },
            )
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|s| s)
            .max_by(|l, r| l.score.cmp(&r.score));
        cache.insert(self.state.clone(), best_solution.clone());
        Ok(best_solution)
    }

    fn solve_max(&mut self) -> Result<(), anyhow::Error> {
        let mut cache = HashMap::new();
        let mut visited = vec![false; self.state.tiles.len()];
        let mut best_score_so_far = 0;
        let solution = self
            .solve_max_dfs(&mut visited, &mut cache, &mut best_score_so_far)?
            .ok_or(anyhow!("failed to compute maximum solution"))?;

        println!("{} {} {}", self.l, self.c, self.v);
        println!("{} {}", solution.deleted_blocks.len(), solution.score);
        solution
            .deleted_blocks
            .into_iter()
            .rev()
            .for_each(|deleted| {
                println!("{} {}", self.l - deleted.l, deleted.c + 1);
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
