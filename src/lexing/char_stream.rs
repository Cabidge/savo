pub struct CharStream {
    source: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
}

impl CharStream {
    pub fn new(src: &str) -> Self {
        CharStream {
            source: src.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn get(&self, index: usize) -> char {
        *self.source.get(index)
            .unwrap_or(&'\0')
    }

    pub fn current(&self) -> char {
        self.get(self.index)
    }

    pub fn take_current(&mut self) -> char {
        let current = self.current();
        self.advance();
        current
    }

    pub fn advance(&mut self) -> char {
        self.index += 1;

        if self.current() == '\n' {
            self.index += 1;
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        self.current()
    }

    pub fn skip_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool
    {
        while predicate(self.current()) {
            self.advance();
        }
    }

    pub fn eat_current(&mut self, expect: char) -> bool {
        if self.current() == expect {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}