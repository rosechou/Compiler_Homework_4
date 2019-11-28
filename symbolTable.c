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
		/*prev = &entry->prevInHashChain->nextInHashChain*/	
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
	}
	entry->nextInHashChain = NULL;
	entry->prevInHashChain = NULL;
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
			SymbolTableEntry* pnext = s->nextInHashChain;
			free(p);
			p=pnext;
		}
	}
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute)
{
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
}

int declaredLocally(char* symbolName)
{
}

void openScope()
{
}

void closeScope()
{
}
