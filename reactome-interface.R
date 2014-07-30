library(RMySQL)

driver <- dbDriver("MySQL")
db <- dbConnect(driver, user="reactome", dbname="reactome_db", host="localhost")
load("dream9.RData")
n.genes <- dim(essentiality)[[1]]
n.samples <- dim(essentiality)[[2]]


make.name.table <- function(table.name, names) {
	deletequery <- "DROP TABLE IF EXISTS "
	deletequery <- paste(deletequery, table.name, sep="")
	deletequery <- paste(deletequery, ";", sep="")
	dbSendQuery(db, deletequery)

	createquery <- "CREATE TABLE "
	createquery <- paste(createquery, table.name, sep="")
	createquery <- paste(createquery, "( geneName varchar(255) );", sep="")
	dbSendQuery(db, createquery)

	for(i in 1:length(names)) {
		name <- names[[i]]
		query <- "INSERT INTO "
		query <- paste(query, table.name, sep="")
		query <- paste(query, " (geneName) VALUES ('", sep="")
		query <- paste(query, name, sep="")
		query <- paste(query, "');", sep="")
		#print(query)
		dbSendQuery(db, query)
	}

}
make.essentiality.id.table <- function() {
	dbSendQuery(db, "DROP TABLE if EXISTS Essentialities_to_id;")
	dbSendQuery(db, "CREATE TABLE Essentialities_to_id AS (
		SELECT ReferenceSequence_2_geneName.geneName, ReferenceSequence_2_geneName.DB_ID
		FROM ReferenceSequence_2_geneName
		INNER JOIN Essentialities
		ON ReferenceSequence_2_geneName.geneName = Essentialities.geneName
		);
	")
}

make.essentiality.keyword.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Essentialities_to_keyword;")
	dbSendQuery(db, "CREATE TABLE Essentialities_to_keyword AS (
		SELECT Essentialities_to_id.geneName, ReferenceSequence_2_keyword.keyword
		FROM ReferenceSequence_2_keyword
		INNER JOIN Essentialities_to_id
		ON Essentialities_to_id.DB_ID = ReferenceSequence_2_keyword.DB_ID
		)
	;")
}
make.expression.id.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Expression_to_id;")
	dbSendQuery(db, "CREATE TABLE Expression_to_id AS (
		SELECT ReferenceSequence_2_geneName.geneName, ReferenceSequence_2_geneName.DB_ID
		FROM ReferenceSequence_2_geneName
		INNER JOIN DreamExpression
		ON DreamExpression.geneName = ReferenceSequence_2_geneName.geneName
		)
	;")
}
make.copynumber.id.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Copynumber_to_id;")
	dbSendQuery(db, "CREATE TABLE Copynumber_to_id AS (
		SELECT ReferenceSequence_2_geneName.geneName, ReferenceSequence_2_geneName.DB_ID
		FROM ReferenceSequence_2_geneName
		INNER JOIN DreamCopynumber
		ON DreamCopynumber.geneName = ReferenceSequence_2_geneName.geneName
		)
	;")
}
make.expression.keyword.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Expression_to_keyword;")
	dbSendQuery(db, "CREATE TABLE Expression_to_keyword AS (
		SELECT Expression_to_id.geneName, ReferenceSequence_2_keyword.keyword
		FROM ReferenceSequence_2_keyword
		INNER JOIN Expression_to_id
		ON Expression_to_id.DB_ID = ReferenceSequence_2_keyword.DB_ID
		)
	;")
}
make.copynumber.keyword.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Copynumber_to_keyword;")
	dbSendQuery(db, "CREATE TABLE Copynumber_to_keyword AS (
		SELECT Copynumber_to_id.geneName, ReferenceSequence_2_keyword.keyword
		FROM ReferenceSequence_2_keyword
		INNER JOIN Copynumber_to_id
		ON Copynumber_to_id.DB_ID = ReferenceSequence_2_keyword.DB_ID
		)
	;")
}

#make a table of essentiality genes and expression genes with the same function
make.same.keyword.expression.table <- function() {
	dbSendQuery(db, "DROP TABLE IF EXISTS Expression_genes_same_keyword;")
	dbSendQuery(db, "CREATE TABLE Expression_genes_same_keyword AS (
		SELECT Essentialities_to_keyword.geneName as EssentialityGene, Expression_to_keyword.geneName as ExpressionGene
		FROM Essentialities_to_keyword
		INNER JOIN Expression_to_keyword
		ON Expression_to_keyword.keyword = Essentialities_to_keyword.keyword
		)
	;")
}

#uncomment as needed
make.all.tables <- function() {
	 #make.name.table("Essentialities", rownames(essentiality))
	 #make.name.table("DreamExpression", rownames(expression))
	 #make.name.table("DreamCopynumber", rownames(copy.number))
	 #make.essentiality.id.table()
	 #make.expression.id.table()
	 #make.copynumber.id.table()
	 #make.expression.keyword.table()
	 #make.copynumber.keyword.table()

	 make.same.keyword.expression.table()

}







