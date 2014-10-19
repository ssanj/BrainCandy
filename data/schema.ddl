CREATE TABLE QUOTE (
     ID SMALLINT NOT NULL AUTO_INCREMENT,
     QUOTE VARCHAR (1000) NOT NULL,
     ATTRIB VARCHAR (100) NOT NULL,
     PRIMARY KEY (id)
) ENGINE=INNODB;

ALTER TABLE QUOTE AUTO_INCREMENT = 10000;

CREATE TABLE ROUND (
    RID SMALLINT NOT NULL,
    QID SMALLINT NOT NULL,
    QDATE DATE NOT NULL,
    FOREIGN KEY (QID) REFERENCES QUOTE(ID),
    PRIMARY KEY (RID, QID)
) ENGINE=INNODB;
