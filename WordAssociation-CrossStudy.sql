PRAGMA foreign_keys = ON;


CREATE TABLE subjects (
  id INTEGER PRIMARY KEY NOT NULL,
  participant VARCHAR(255)
);

CREATE TABLE response_behaviors (
  id INTEGER PRIMARY KEY NOT NULL,
  response_order INTEGER NOT NULL,
  cue_order INTEGER NOT NULL,
  study_id INTEGER NOT NULL,
  subject_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id),
  FOREIGN KEY (study_id) REFERENCES study(id)
);


CREATE TABLE responses (
  id INTEGER PRIMARY KEY NOT NULL,
  response TEXT NOT NULL
);

CREATE TABLE responses_study (
  id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  FOREIGN KEY (study_id) REFERENCES study(id),
  FOREIGN KEY (response_id) REFERENCES responses(id)
);

CREATE TABLE cues (
  id INTEGER PRIMARY KEY NOT NULL,
  cue TEXT NOT NULL
);

CREATE TABLE cues_study (
  id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  FOREIGN KEY (study_id) REFERENCES study(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id)
);

CREATE TABLE cues_responses (
  id INTEGER PRIMARY KEY NOT NULL,
  cue_study_id INTEGER NOT NULL,
  response_study_id INTEGER NOT NULL,
  FOREIGN KEY (cue_study_id) REFERENCES cues_study(id),
  FOREIGN KEY (response_study_id) REFERENCES responses_study(id)
);


CREATE TABLE response_map (
  id INTEGER PRIMARY KEY NOT NULL,
  cue_response_id INTEGER NOT NULL,
  kuperman_id INTEGER,
  subtlex_id INTEGER,
  cue_id INTEGER,
  revision TEXT,
  researcher_id INTEGER,
  timestamp TEXT,
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
  (3, "Meghan",  "Garcelon", "mgarce3@lsu.edu"),
  (4, "Angelina", "Chauvin",  "achau22@lsu.edu"),
  (5, "Hannah", "Pedigo", "hpedig2@lsu.edu"),
  (6, "Sophie", "Vidrine", "svidri8@lsu.edu"),
  (7, "Francesca", "Thomassee", "fthom22@lsu.edu"),
  (8, "Erin",     "Jines",    "ejines1@lsu.edu"),
  (9, "Trinity",    "Phipps",     "tphipp4@lsu.edu"),
  (10, "Daniela",  "Ucles", "ducles1@lsu.edu"),
  (11, "Marissa", "Goldthorp",  "mgoldt1@lsu.edu"),
  (12, "Camila", "Astete", "castet2@lsu.edu"),
  (13, "Layla", "Canaday", "lcanad2@lsu.edu")
;


CREATE TABLE words_meta (
  word TEXT NOT NULL,
  kuperman_id INTEGER,
  subtlex_id INTEGER,
  cue_id INTEGER,
  FOREIGN KEY (kuperman_id) REFERENCES kuperman(id),
  FOREIGN KEY (subtlex_id) REFERENCES subtlex(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id)
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


CREATE TABLE study (
  id INTEGER PRIMARY KEY NOT NULL,
  study TEXT
)


