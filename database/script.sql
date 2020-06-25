-- db.`User` definition

CREATE TABLE `User` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(100) NOT NULL,
  `password` varchar(100) NOT NULL,
  `email` varchar(100) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `User_UN` (`email`)
);


-- db.Dataset definition
CREATE TABLE `Dataset` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) NOT NULL,
  `description` varchar(500) DEFAULT NULL,
  `dataset` blob NOT NULL,
  `user_id` int(11) NOT NULL,
  `rootPath` varchar(100) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `Dataset_FK` (`user_id`),
  CONSTRAINT `Dataset_FK` FOREIGN KEY (`user_id`) REFERENCES `User` (`id`)
);


-- db.Model definition

CREATE TABLE `Model` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(100) NOT NULL,
  `description` varchar(100) DEFAULT NULL,
  `user_id` int(11) NOT NULL,
  `dummy_model` blob,
  `center_model` blob,
  `impute_model` blob,
  `target` varchar(100) DEFAULT NULL,
  `dataset_id` int(11) NOT NULL,
  `trainRowNumbers` blob NOT NULL,
  `models` blob NOT NULL,
  `cols` blob NOT NULL,
  `isPublic` int(1) NOT NULL DEFAULT '0',
  `rootPath` varchar(100) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `Model_FK` (`user_id`),
  KEY `Model_FK_1` (`dataset_id`),
  CONSTRAINT `Model_FK` FOREIGN KEY (`user_id`) REFERENCES `User` (`id`),
  CONSTRAINT `Model_FK_1` FOREIGN KEY (`dataset_id`) REFERENCES `Dataset` (`id`)
);


-- db.Model_User definition

CREATE TABLE `Model_User` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `model_id` int(11) NOT NULL,
  `user_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `Model_User_FK` (`model_id`),
  KEY `Model_User_FK_1` (`user_id`),
  CONSTRAINT `Model_User_FK` FOREIGN KEY (`model_id`) REFERENCES `Model` (`id`),
  CONSTRAINT `Model_User_FK_1` FOREIGN KEY (`user_id`) REFERENCES `User` (`id`)
);