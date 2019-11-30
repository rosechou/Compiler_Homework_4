#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

int HASH(char * str) {
	int idx=0;
	while (*str){
		idx = idx << 1;
		idx+=*str;
		str++;
	}
	return (idx & (HASH_TABLE_SIZE-1));
}

SymbolTable symbolTable;

SymbolTableEntry* newSymbolTableEntry(int nestingLevel)
{
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}

void removeFromHashTrain(int hashIndex, SymbolTableEntry* entry)
{
	SymbolTableEntry** prev;
	if (entry->prevInHashChain)
	{
		prev = &entry;
	}
	else
	{
		prev = &symbolTable.hashTable[hashIndex];
	}
	
	
	/*point to the next of goal*/
	*prev = entry->nextInHashChain;

	if(entry->nextInHashChain)
	{/*let goal of next point to goal of prev*/
		entry->nextInHashChain->prevInHashChain = entry->prevInHashChain;
		/*TODO*/
		entry->prevInHashChain->nextInHashChain = entry->nextInHashChain;
		/*how to link nextInSameLevel & samenameinouterlevel
		if(entry->nextInSameLevel)
		{
				
		}
		*/
	}
	else
	{
		/*TODO:pre point to null*/
		entry->prevInHashChain->nextInHashChain=NULL;

	}
	/*TODO:free*/
	entry->nextInHashChain = NULL;
	entry->prevInHashChain = NULL;
	free(entry);
}

void enterIntoHashTrain(int hashIndex, SymbolTableEntry* entry)
{
	SymbolTableEntry** head = &symbolTable.hashTable[hashIndex];
	//insert entry become new head
	if (*head)
	{
		(*head)->prevInHashChain = entry;
		entry -> nextInHashChain = *head;
	}
	/*point to new entry*/
	*head = entry;
}

void initializeSymbolTable()
{
	/*let symbol initialize to empty*/
	memset(&symbolTable, 0, sizeof(SymbolTable));
}

void symbolTableEnd()
{
	/*free all entry in the scope*/
	for(int i=0; i< HASH_TABLE_SIZE;i++)
	{
		SymbolTableEntry* p = symbolTable.hashTable[i];
		while(p!=NULL)
		{
			SymbolTableEntry* pnext = p->nextInHashChain;
			free(p);
			p=pnext;
		}
	}
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
	SymbolTableEntry* goal=symbolTable.hashTable[HASH(symbolName)];
	while(goal!=NULL)
	{
		if(strcmp(symbolName, goal->name)==0)
		{
			return goal;
		}
		goal = goal->nextInHashChain;
	}
	return NULL;
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute)
{
	int index = HASH(symbolName);
	SymbolTableEntry* new = newSymbolTableEntry(symbolTable.currentLevel);
	new->name = strdup(symbolName);
	new->attribute = attribute;
	enterIntoHashTrain(index, new);
	return new;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
	int index = HASH(symbolName);
	SymbolTableEntry* entry = symbolTable.hashTable[index]->nextInHashChain;
	while(entry != NULL)
	{
		if(strcmp(entry->name, symbolName)==0)
		{
			removeFromHashTrain(index, entry);
			return;	
		}
		entry = entry->nextInHashChain;
	}
}

int declaredLocally(char* symbolName)
{
	int index = HASH(symbolName);
	SymbolTableEntry* entry = symbolTable.hashTable[index];

	while(entry)	
	{
		if(strcmp(entry->name, symbolName)==0)
		{
			return(entry->nestingLevel == symbolTable.currentLevel);
		}
	}
	return 0;
}

void openScope()
{
	symbolTable.currentLevel++;
	symbolTable.scopeDisplay[symbolTable.currentLevel]= NULL;
}

void closeScope()
{
	SymbolTableEntry* rm = symbolTable.scopeDisplay[symbolTable.currentLevel];
	while(rm != NULL)
	{
		SymbolTableEntry* temp = rm;
		rm = rm -> nextInSameLevel;
		free(temp);
	}
	symbolTable.currentLevel--;
}
