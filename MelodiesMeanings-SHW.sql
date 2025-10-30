PRAGMA foreign_keys = ON;

CREATE TABLE response_behaviors (
  id INTEGER PRIMARY KEY NOT NULL,
  participant VARCHAR(255) NOT NULL,
  cue_id INTEGER NOT NULL,
  response_order INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id)
);


CREATE TABLE responses (
  id INTEGER PRIMARY KEY NOT NULL,
  response TEXT NOT NULL
);


CREATE TABLE cues_responses (
  id INTEGER PRIMARY KEY NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id)
);


CREATE TABLE response_map (
  id INTEGER PRIMARY KEY NOT NULL,
  cue_response_id TEXT NOT NULL,
  kuperman_id INTEGER,
  subtlex_id INTEGER,
  cue_id INTEGER,
  revision TEXT,
  researcher_id INTEGER,
  timestamp TEXT NOT NULL,
  FOREIGN KEY(cue_response_id) REFERENCES cues_responses(id),
  FOREIGN KEY(kuperman_id) REFERENCES kuperman(id),
  FOREIGN KEY(subtlex_id) REFERENCES subtlex(id),
  FOREIGN KEY(cue_id) REFERENCES cues(id),
  FOREIGN KEY(researcher_id) REFERENCES researchers(id)
);


CREATE TABLE researchers (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);
INSERT INTO researchers (id, first_name, last_name, email)
VALUES
  (1, "Stan",     "West",    "swest19@lsu.edu"),
  (2, "Chris",    "Cox",     "chriscox@lsu.edu"),
  (3, "Caudell",  "Collins", "ccol144@lsu.edu"),
  (4, "Cristina", "Garcia",  "cgarc91@lsu.edu"),
  (5, "Ryan", "Ferguson", "rferg18@lsu.edu"),
  (6, "Jade", "Ford", "jford41@lsu.edu"),
  (7, "Will", "Decker", "jdecke5@lsu.edu")
;


CREATE TABLE cues (
  id INTEGER PRIMARY KEY,
  cue TEXT NOT NULL
);


CREATE TABLE words_meta (
  word TEXT NOT NULL,
  kuperman_id INTEGER,
  subtlex_id INTEGER,
  cue_id INTEGER,
  FOREIGN KEY (kuperman_id) REFERENCES kuperman(id),
  FOREIGN KEY (subtlex_id) REFERENCES subtlex(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id)
);


CREATE TABLE subject_decisions (
  id INTEGER PRIMARY KEY,
  subject_id INTEGER NOT NULL,
  decision_id INTEGER NOT NULL,
  researcher_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
  FOREIGN KEY (decision_id) REFERENCES decisions(id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id)
);

CREATE TABLE subject_locks (
  id INTEGER PRIMARY KEY,
  hash TEXT NOT NULL UNIQUE,
  subject_id INTEGER NOT NULL,
  researcher_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id)
);

CREATE TABLE response_locks (
  id INTEGER PRIMARY KEY,
  hash TEXT NOT NULL UNIQUE,
  cue_response_id INTEGER NOT NULL,
  researcher_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (cue_response_id) REFERENCES cues_responses(id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id)
);


CREATE TABLE decisions (
  id INTEGER PRIMARY KEY NOT NULL,
  decision TEXT NOT NULL
);
INSERT INTO decisions (id, decision)
VALUES (1, "ACCEPT"), (2, "REJECT"), (3, "UNCERTAIN");


CREATE TABLE kuperman (
  id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  aoa REAL NOT NULL
);


CREATE TABLE subtlex (
  id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  Lg10WF REAL NOT NULL,
  Lg10CD REAL NOT NULL
);