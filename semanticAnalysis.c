#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;

/*0*/
void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    /*TODO*/
    switch(errorMsgKind)
    {
	case SYMBOL_UNDECLARED:
		printf("ID <%s> undeclared.\n", name2);
		break;
	case SYMBOL_REDECLARE:
		printf("ID <%s> redeclared.\n", name2);
		break;
	case TOO_MANY_ARGUMENTS:
		printf("too few arguments to function <%s>.\n", name2);
		break;
	case TOO_FEW_ARGUMENTS:
		printf("too many arguments to function <%s>.\n", name2);
		break;
	case PASS_ARRAY_TO_SCALAR:
		printf("Array <%s> passed to scalar parameter <%s>.\n", node1->semantic_value.identifierSemanticValue.identifierName, name2);
		break;
	case PASS_SCALAR_TO_ARRAY:
		printf("Scalar <%s> passed to array parameter <%s>.\n", node1->semantic_value.identifierSemanticValue.identifierName, name2);
		break;
	case SYMBOL_IS_NOT_TYPE:
            printf("Type <%s> is not a valid type", name2);
            break;
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    
}

/*0*/
void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    /*TODO*/
    switch(errorMsgKind)
    {
	case RETURN_TYPE_UNMATCH:
		puts("Incompatible return type.");
		break;
	case INCOMPATIBLE_ARRAY_DIMENSION:
		puts("Incompatible array dimensions.");
		break;
	case ARRAY_SUBSCRIPT_NOT_INT:
		puts("Array subscript is not an integer");
		break;
	case NOT_FUNCTION_NAME:     
		puts("ID is not a function."); 
		break;
	 case PARAMETER_TYPE_UNMATCH:
        	puts("Parameter is incompatible with parameter type.");
        	break;
    case IS_TYPE_NOT_VARIABLE:  
    		puts("ID is a type, not a variable."); 
    		break;
    case IS_FUNCTION_NOT_VARIABLE:
            puts("ID is a function, not a variable."); 
            break;
    case NOT_ARRAY:             
    		puts("ID is not an array."); 
    		break;
    case EXCESSIVE_ARRAY_DIM_DECLARATION:
        	puts("ID array dimension cannot be greater than noraml.");
        	break;
    case ARRAY_SIZE_NOT_INT:
        puts("Size of array has non-integer type.");
        break;
    case ARRAY_SIZE_NEGATIVE:
        puts("Size of array is negative.");
        break;
	default:
        	printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        	break;
    }
    
}


void semanticAnalysis(AST_NODE *root)
{
    processProgramNode(root);
}


DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}

/*0*/
void processProgramNode(AST_NODE *programNode)
{
	AST_NODE* child = programNode->child;
	while(child)
	{
		processDeclarationNode(child);
		child = child->rightSibling;	
	}
	symbolTableEnd();
}

/*0*/
void processDeclarationNode(AST_NODE* declarationNode)
{
	AST_NODE* child = declarationNode->child;
	AST_NODE* now = declarationNode;
	if(declarationNode->nodeType == VARIABLE_DECL_LIST_NODE)
	{
		while(child!=NULL)
		{
			processDeclarationNode(child);
			child = child->rightSibling;
		}
	}
	else
	{
		switch(declarationNode->semantic_value.declSemanticValue.kind)
		{
			case FUNCTION_DECL:
				declareFunction(declarationNode);
			case VARIABLE_DECL:
				declareIdList(declarationNode, declarationNode->semantic_value.declSemanticValue.kind,0);
			case TYPE_DECL:
				declareIdList(declarationNode, declarationNode->semantic_value.declSemanticValue.kind,0);
			case FUNCTION_PARAMETER_DECL:
				declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 1);
		}
	}
}

/*2*/
void processTypeNode(AST_NODE* idNodeAsType)
{
	IdentifierSemanticValue* val = &idNodeAsType->semantic_value.identifierSemanticValue;
	char* id = val->identifierName;
	SymbolTableEntry* entry = retrieveSymbol(id);

	if(!entry)
	{
		printErrorMsg(idNodeAsType, SYMBOL_UNDECLARED);
		idNodeAsType->dataType = ERROR_TYPE;
	}
	else if(entry -> attribute -> attributeKind != TYPE_ATTRIBUTE)
	{
		printErrorMsg(idNodeAsType, SYMBOL_IS_NOT_TYPE);
		idNodeAsType->dataType = ERROR_TYPE;
	}
	else
	{
		val -> symbolTableEntry = entry;
		TypeDescriptor* typeDesc = entry->attribute->attr.typeDescriptor;
        	switch (typeDesc->kind) 
        	{
        		case SCALAR_TYPE_DESCRIPTOR:
            			idNodeAsType->dataType = typeDesc->properties.dataType;
            			break;
        		case ARRAY_TYPE_DESCRIPTOR:
            			idNodeAsType->dataType = typeDesc->properties.arrayProperties.elementType;
            			break;
            }
	}
}


/**/
void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize)
{
	
}

/*0.5*/
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
	if(assignOrExprRelatedNode->nodeType==STMT_NODE)
	{
		if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
		{
			checkAssignmentStmt(assignOrExprRelatedNode);
		}
		else if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT)
		{
			checkFunctionCall(assignOrExprRelatedNode);	
		}	
	}
	else
	{
		processExprRelatedNode(assignOrExprRelatedNode);	
	}
}
/*0*/
void checkWhileStmt(AST_NODE* whileNode)
{
	/*the child of whilenode is the stmt in ()*/
	if(whileNode->child->nodeType == EXPR_NODE)
	{
		processExprNode(whileNode->child);	
	}
	else if(whileNode->child->nodeType == IDENTIFIER_NODE)
	{
		/*too many arguement*/
		processVariableRValue(whileNode->child);/*change arguement*/
	}
	else if(whileNode->child->nodeType == STMT_NODE)
	{
		checkFunctionCall(whileNode->child);
	}
	else
	{
		processConstValueNode(whileNode->child);
	}
	/*to access content inside of brace*/
	processBlockNode(whileNode->child->rightSibling);
}


void checkForStmt(AST_NODE* forNode)
{
	AST_NODE* init = forNode->child;
	AST_NODE* cond = init->rightSibling;
	AST_NODE* loop = cond->rightSibling;
	processGeneralNode(init);
	processGeneralNode(cond);
	processGeneralNode(loop);
	processBlockNode(loop->rightSibling);
}


void checkAssignmentStmt(AST_NODE* assignmentNode)
{
	AST_NODE* left = assignmentNode->child;
	AST_NODE* right = left->rightSibling;
	processVariableLValue(left);
	processVariableRValue(right);
}


void checkIfStmt(AST_NODE* ifNode)
{
	AST_NODE* cond = ifNode->child;
	AST_NODE* block = cond->rightSibling;
	AST_NODE* else_block = block->rightSibling;
	
	checkAssignOrExpr(cond);
	processStmtNode(block);
	processStmtNode(else_block);
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
	AST_NODE* func_name = functionCallNode->child;
	AST_NODE* para_list = func_name->rightSibling;
	AST_NODE* actual_para = para_list->child;
	int regular_count=0;
	int actual_count = 0;
	SymbolTableEntry* entry = retrieveSymbol(functionCallNode->child->semantic_value.identifierSemanticValue.identifierName);

	while((entry!=NULL)&&(entry->attribute->attributeKind != FUNCTION_SIGNATURE))
	{
		entry = entry->sameNameInOuterLevel;/*TODO: check pointer in symbolTable*/
	}
	
	if(entry == NULL)
	{
		printErrorMsgSpecial(functionCallNode->child, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, SYMBOL_UNDECLARED);
		return;	
	}

	regular_count = entry->attribute->attr.functionSignature->parametersCount;
	while(actual_para != NULL)
	{
		actual_count++;
		actual_para = actual_para->rightSibling;	
	}	
	
	if(regular_count < actual_count)
	{
		printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_MANY_ARGUMENTS);
		return;
	}
	else(regular_count > actual_count)
	;{
		printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_FEW_ARGUMENTS);
		return;
	}	
	functionCallNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
}

void checkFunctionCall(AST_NODE* functionCallNode)
{
	AST_NODE* func_name = functionCallNode->child;
	AST_NODE* para_list = func_name->rightSibling;
	AST_NODE* actual_para = para_list->child;
	char* id = func_name->semantic_value.identifierSemanticValue.identifierName;
	SymbolTableEntry* entry = retrieveSymbol(id);
	Parameter* parameter;

	checkWriteFunction(functionCallNode);
	
	if(!entry)
	{
		printErrorMsgSpecial(functionCallNode->child, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, SYMBOL_UNDECLARED);
		functionCallNode->dataType = ERROR_TYPE;
	}else if(entry->attribute->attributeKind != FUNCTION_SIGNATURE)
	{
		printErrorMsg(func_name, NOT_FUNCTION_NAME);
		functionCallNode->dataType = ERROR_TYPE;
	}

	processGeneralNode(para_list);
	parameter = entry->attribute->attr.functionSignature->parameterList;

	while(actual_para && parameter)
	{
		if(actual_para->dataType == ERROR_TYPE)
		{
			functionCallNode->dataType = ERROR_TYPE;
		}
		checkParameterPassing(parameter, actual_para);
		parameter= parameter->next;
		actual_para = actual_para->rightSibling;
	}

	if(actual_para)
	{
		printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_MANY_ARGUMENTS);
	}else if(parameter)
	{
		printErrorMsgSpecial(functionCallNode, functionCallNode->child->semantic_value.identifierSemanticValue.identifierName, TOO_FEW_ARGUMENTS);
	}
	return;
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{
	 
	int actual_para_flag = 0;
	DATA_TYPE actual_para_type = actualParameter->dataType;
	
	if(actual_para_type == INT_PTR_TYPE || actual_para_type == FLOAT_PTR_TYPE)
	{
		actual_para_flag = 0;
	}
	
	if(formalParameter->type->kind == SCALAR_TYPE_DESCRIPTOR && actual_para_type)
	{
		printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_ARRAY_TO_SCALAR);
	}
	else if(formalParameter->type->kind == ARRAY_TYPE_DESCRIPTOR && !(actual_para_type))
	{
		printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
	}
	else if(actual_para_type == CONST_STRING_TYPE)
	{
		printErrorMsg(actualParameter, PARAMETER_TYPE_UNMATCH);
		actual_para_type = ERROR_TYPE;
	}
}

/*2*/
void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
	switch(exprRelatedNode->nodeType)
	{
		case CONST_VALUE_NODE:
			processConstValueNode(exprRelatedNode);
			break;
		case IDENTIFIER_NODE:
			processVariableRValue(exprRelatedNode);
			break;
		case EXPR_NODE:
			processExprNode(exprRelatedNode);
			break;
		case STMT_NODE:
			checkFunctionCall(exprRelatedNode);
			break;
		default:
			printf("unhandled case\n");
			exprRelatedNode->dataType = ERROR_TYPE;
			break;
	}	
}
/*0.5*/
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
	if(exprOrConstNode->nodeType == CONST_VALUE_NODE)
	{
		if(exprOrConstNode->dataType == INT_TYPE)
		{
			if(fValue)
			{
				*fValue = exprOrConstNode->semantic_value.const1->const_u.intval;
			}
			else
			{
				*iValue = exprOrConstNode->semantic_value.const1->const_u.intval;
			}
		}
		else
		{
			*fValue = exprOrConstNode->semantic_value.const1->const_u.fval;
		}
	}
	else
	{
		if(exprOrConstNode->dataType == INT_TYPE)
		{
			if(fValue)
			{
				*fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
			}
			else
			{
				*iValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
			}
		}
		else
		{
			*fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.fValue;

		}
	}
}

void evaluateExprValue(AST_NODE* exprNode)
{
	if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION)
	{
		AST_NODE* left = exprNode->child;
		AST_NODE* right = exprNode->rightSibling;
		if(left->dataType == INT_TYPE && right->dataType == INT_TYPE)
		{
				int l_value = 0;
				int r_value = 0;
				getExprOrConstValue(left, &l_value, NULL);
				getExprOrConstValue(right, &r_value, NULL);
				exprNode->dataType = INT_TYPE; /*const folding*/

				switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
				{
					case BINARY_OP_ADD:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value + r_value);
						break;
					case BINARY_OP_SUB:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value - r_value);
						break;
					case BINARY_OP_MUL:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value * r_value);
						break;
					case BINARY_OP_DIV:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value / r_value);
						break;
					case BINARY_OP_EQ:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value == r_value);
						break;
					case BINARY_OP_GE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value >= r_value);
                        break;
                    case BINARY_OP_LE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value <= r_value);
                        break;
                    case BINARY_OP_NE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value != r_value);
                        break;
                    case BINARY_OP_GT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value > r_value);
                        break;
                    case BINARY_OP_LT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value < r_value);
						break;
					case BINARY_OP_AND:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value && r_value);
                        break;
                    case BINARY_OP_OR:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = (l_value || r_value);
                        break;
					default:
						printf("unhandled case\n");
						break;
				}
		return;
		}
		else
		{
				float l_value = 0.0;
				float r_value = 0.0;
				getExprOrConstValue(left, NULL, &l_value);
				getExprOrConstValue(right, NULL, &r_value);
				exprNode->dataType = FLOAT_TYPE; /*const folding*/

				switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
				{
					case BINARY_OP_ADD:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value + r_value);
						break;
					case BINARY_OP_SUB:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value - r_value);
						break;
					case BINARY_OP_MUL:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value * r_value);
						break;
					case BINARY_OP_DIV:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value / r_value);
						break;
					case BINARY_OP_EQ:
						exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value == r_value);
						break;
					case BINARY_OP_GE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value >= r_value);
                        break;
                    case BINARY_OP_LE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value <= r_value);
                        break;
                    case BINARY_OP_NE:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value != r_value);
                        break;
                    case BINARY_OP_GT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value > r_value);
                        break;
                    case BINARY_OP_LT:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value < r_value);
						break;
					case BINARY_OP_AND:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value && r_value);
                        break;
                    case BINARY_OP_OR:
                        exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = (l_value || r_value);
                        break;
					default:
						printf("unhandled case\n");
						break;
				}
			return;
		}
	}
	else
	{
		AST_NODE* node = exprNode->child;
		if(node->dataType == INT_TYPE)
		{
			int value=0;
			getExprOrConstValue(node, &value, NULL);
			exprNode->dataType = INT_TYPE;
			switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
			{
				case UNARY_OP_POSITIVE:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = value;
					break;
				case UNARY_OP_NEGATIVE:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = -(value);
					break;
				case UNARY_OP_LOGICAL_NEGATION:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = !(value);
					break;
				default:
					printf("unhandled case\n");
					break;
			}
		}
		else
		{
			float value = 0.0;
			getExprOrConstValue(node, NULL, &value);
			exprNode->dataType = FLOAT_TYPE;
			switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
			{
				case UNARY_OP_POSITIVE:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = value;
					break;
				case UNARY_OP_NEGATIVE:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = -(value);
					break;
				case UNARY_OP_LOGICAL_NEGATION:
					exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = !(value);
					break;
				default:
					printf("unhandled case\n");
					break;
			}
		}
	}

}


void processExprNode(AST_NODE* exprNode)
{	
	if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION)
	{
		AST_NODE* left = exprNode->child;
		AST_NODE* right = exprNode->rightSibling;

		processExprRelatedNode(left);
		processExprRelatedNode(right);

		if (left->dataType == CONST_STRING_TYPE || right->dataType == CONST_STRING_TYPE) 
		{
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }
		if(left->dataType == INT_PTR_TYPE || left->dataType == FLOAT_PTR_TYPE)
		{
			printErrorMsg(left, INCOMPATIBLE_ARRAY_DIMENSION);
			exprNode->dataType= ERROR_TYPE;	
		}
		else if(right->dataType == INT_PTR_TYPE || right->dataType == FLOAT_PTR_TYPE)
		{
			printErrorMsg(right, INCOMPATIBLE_ARRAY_DIMENSION);
			exprNode->dataType= ERROR_TYPE;	
		}


		if(left->dataType == ERROR_TYPE || right->dataType == ERROR_TYPE)
        {
            exprNode->dataType = ERROR_TYPE;
        }

        switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
        {
        	case BINARY_OP_GT: case BINARY_OP_LT: case BINARY_OP_EQ: case BINARY_OP_GE: case BINARY_OP_LE: case BINARY_OP_NE: case BINARY_OP_AND: case BINARY_OP_OR:
        		exprNode->dataType = INT_TYPE;
        		break;
        	default:
        		exprNode->dataType = getBiggerType(left->dataType, right->dataType);
        		break;
        }
    }
    else
    {
    	AST_NODE* node = exprNode->child;
    	processExprRelatedNode(node);

		if (node->dataType == CONST_STRING_TYPE) 
		{
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }
        if(node->dataType == INT_PTR_TYPE || node->dataType == FLOAT_PTR_TYPE)
        {
        	printErrorMsg(node, INCOMPATIBLE_ARRAY_DIMENSION);
			exprNode->dataType= ERROR_TYPE;	
        }
        if(node->dataType != INT_TYPE && node->dataType != FLOAT_TYPE)
        {
        	exprNode->dataType= ERROR_TYPE;
        }
    }
}

void processVariableLValue(AST_NODE* idNode)
{
	SymbolTableEntry* entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	idNode->semantic_value.identifierSemanticValue.symbolTableEntry=entry;
	if (!entry)
	{
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
		idNode->dataType=ERROR_TYPE;
	}

	SymbolAttributeKind kind = entry->attribute->attributeKind;

	if(kind == FUNCTION_SIGNATURE)
	{
		printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
		idNode->dataType = ERROR_TYPE;
		return;
	}else if(kind == TYPE_ATTRIBUTE)
	{
		printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
		idNode->dataType = ERROR_TYPE;
		return;
	}
	TypeDescriptor* typdes= idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;

	switch(idNode->semantic_value.identifierSemanticValue.kind)
	{
		case NORMAL_ID:
			if (typdes->kind == ARRAY_TYPE_DESCRIPTOR)
			{
				printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
				idNode->dataType = ERROR_TYPE;
			}
			else if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
			{
				idNode->dataType = typdes->properties.dataType;
			}
			break;

		case ARRAY_ID:
			if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
			{
				printErrorMsg(idNode, NOT_ARRAY);
				idNode->dataType = ERROR_TYPE;
				break;
			}
			else if(typdes->kind == ARRAY_TYPE_DESCRIPTOR)
			{
				int dim = 0;
				AST_NODE* dimNode = idNode->child;

				while(dimNode)
				{
					dim++;
					processExprRelatedNode(dimNode);
					if(dimNode->dataType == ERROR_TYPE)
					{
						idNode->dataType = ERROR_TYPE;
					}
					else if(dimNode ->dataType == FLOAT_TYPE)
					{
						printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
						idNode->dataType = ERROR_TYPE;
					}
					dimNode= dimNode ->rightSibling;
				}

				if(dim == typdes->properties.arrayProperties.dimension)
				{
					idNode->dataType = typdes->properties.arrayProperties.elementType;
				}	
				else
				{
					printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
					idNode->dataType=ERROR_TYPE;
				}
			}
			break;

		default:
			break;

	}

}

void processVariableRValue(AST_NODE* idNode)
{
	SymbolTableEntry* entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
	idNode->semantic_value.identifierSemanticValue.symbolTableEntry=entry;
	if (!entry)
	{
		printErrorMsg(idNode, SYMBOL_UNDECLARED);
		idNode->dataType=ERROR_TYPE;
	}

	SymbolAttributeKind kind = entry->attribute->attributeKind;

	if(kind == FUNCTION_SIGNATURE)
	{
		printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
		idNode->dataType = ERROR_TYPE;
		return;
	}else if(kind == TYPE_ATTRIBUTE)
	{
		printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
		idNode->dataType = ERROR_TYPE;
		return;
	}
	TypeDescriptor* typdes= idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;

	switch(idNode->semantic_value.identifierSemanticValue.kind)
	{
		case NORMAL_ID:
			if(typdes->kind == ARRAY_TYPE_DESCRIPTOR)
			{
				if(typdes->properties.arrayProperties.elementType == INT_TYPE)
				{
					idNode->dataType = INT_PTR_TYPE;
				}
				else if(typdes->properties.arrayProperties.elementType == FLOAT_TYPE)
				{
					idNode->dataType = FLOAT_PTR_TYPE;
				}
			}
			else if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
			{
				idNode->dataType = typdes->properties.dataType;
			}
			break;

		case ARRAY_ID:
			if(typdes->kind == SCALAR_TYPE_DESCRIPTOR)
			{
				printErrorMsg(idNode, NORMAL_ID);
				idNode->dataType = ERROR_TYPE;
			}
			else if(typdes->kind == ARRAY_TYPE_DESCRIPTOR)
			{
				int dim = 0;
				AST_NODE* dimNode = idNode->child;
				while(dimNode)
				{
					dim++;
					processExprRelatedNode(dimNode);
					if(dimNode->dataType == ERROR_TYPE)
					{
						idNode->dataType = ERROR_TYPE;
					}
					else if(dimNode ->dataType == FLOAT_TYPE)
					{
						printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
						idNode->dataType = ERROR_TYPE;
					}
					dimNode= dimNode ->rightSibling;
				}

				ArrayProperties* arrpro = &typdes->properties.arrayProperties;
				int dim_check = arrpro->dimension;

				if(dim == dim_check)
				{
					idNode->dataType = arrpro->elementType;
				}	
				else if(dim < dim_check)/*array slice*/
				{
					if(arrpro->elementType == INT_TYPE)
					{
						idNode->dataType = INT_PTR_TYPE;
					}
					else if(arrpro->elementType == FLOAT_TYPE)
					{
						idNode->dataType = FLOAT_PTR_TYPE;
					}
					
				}
				else
				{
					printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
					idNode->dataType=ERROR_TYPE;
				}
			}
			break;
	}
}


void processConstValueNode(AST_NODE* constValueNode)
{
	switch(constValueNode->semantic_value.const1->const_type)
	{
		case INTEGERC:
			constValueNode->dataType = INT_TYPE;
			constValueNode->semantic_value.exprSemanticValue.constEvalValue.iValue = constValueNode->semantic_value.const1->const_u.intval;
			break;
		case FLOATC:
			constValueNode->dataType = FLOAT_TYPE;
			constValueNode->semantic_value.exprSemanticValue.constEvalValue.fValue = constValueNode->semantic_value.const1->const_u.fval;
			break;
		case STRINGC:
			constValueNode->dataType = CONST_STRING_TYPE;
			break;
		default:
			constValueNode->dataType = ERROR_TYPE;
			break;
	}
}


void checkReturnStmt(AST_NODE* returnNode)
{
	AST_NODE* parent = returnNode->parent;
	AST_NODE* type;
	AST_NODE* child = returnNode->child;

	while(parent)
	{
		if(parent->nodeType == DECLARATION_NODE)
		{
			if(parent->semantic_value.declSemanticValue.kind == FUNCTION_DECL)
			{
				break;
			}
		}
		parent = parent->parent;
	}

	type = parent->child;
	if(type->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType == VOID_TYPE)
	{
		if(child != NULL)
		{
			printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
		}
		return;
	}
	else if(child->nodeType == EXPR_NODE)
	{
		processExprNode(child);
	}
	else if(child->nodeType == IDENTIFIER_NODE)
	{
		processVariableRValue(child);/*change arguement*/
	}
	else if(child->nodeType == STMT_NODE)
	{
		checkFunctionCall(child);
	}
	else
	{
		processConstValueNode(child);
	}

	if(type->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType!= child->dataType)
	{
		printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
	}
}


void processBlockNode(AST_NODE* blockNode)
{
	openScope();
	AST_NODE* child = blockNode->child;
	while(child != NULL)
	{
		processGeneralNode(child);
		child = child->rightSibling;
	}
	closeScope();	
}

void processStmtNode(AST_NODE* stmtNode)
{
	if(stmtNode->nodeType == NUL_NODE)
	{
		return;
	}
	else if(stmtNode->nodeType == BLOCK_NODE)
	{
		return processBlockNode(stmtNode);
	}

	switch(stmtNode->semantic_value.stmtSemanticValue.kind)
	{
		case ASSIGN_STMT:
			return checkAssignmentStmt(stmtNode);
		case WHILE_STMT:
			return checkWhileStmt(stmtNode);
		case FOR_STMT:
			return checkForStmt(stmtNode);
		case IF_STMT:
			return checkIfStmt(stmtNode);
		case FUNCTION_CALL_STMT:
			return checkFunctionCall(stmtNode);
		case RETURN_STMT:
			return checkReturnStmt(stmtNode);
		default:
			stmtNode->dataType = ERROR_TYPE;
			return;
	}
}

void processGeneralNode(AST_NODE *node)
{
	AST_NODE* child = node->child;
	switch(node->nodeType)
	{
		case VARIABLE_DECL_LIST_NODE:
			while(child!=NULL)
			{
				processDeclarationNode(child);
				if(child->dataType == ERROR_TYPE)
				{
					node->dataType = ERROR_TYPE;
				}
				child = child->rightSibling;
			}
			break;
		case STMT_LIST_NODE:
			while(child!=NULL)
			{
				processStmtNode(child);
				if(child->dataType == ERROR_TYPE)
				{
					node->dataType = ERROR_TYPE;
				}
				child = child->rightSibling;
			}
			break;
		case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
			while(child!=NULL)
			{
				checkAssignOrExpr(child);
				if(child->dataType == ERROR_TYPE)
				{
					node->dataType = ERROR_TYPE;
				}
				child = child->rightSibling;
			}
			break;
		case NONEMPTY_RELOP_EXPR_LIST_NODE:
			while(child!=NULL)
			{
				processExprRelatedNode(child);
				if(child->dataType == ERROR_TYPE)
				{
					node->dataType = ERROR_TYPE;
				}
				child = child->rightSibling;
			}
			break;
		case NUL_NODE:
			break;
		default:
			puts("unhandled case.");
			node->nodeType = ERROR_TYPE;
			break;
	}
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize)
{
	AST_NODE* list = idNode->child;
	AST_NODE* dim = list;
	typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
	int dimension = 0;
	if((dim->nodeType == NUL_NODE)&&(ignoreFirstDimSize))
	{
		typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension] =0;
		dimension++;
		dim = list->rightSibling;
	}
	while(dim)
	{
		if(dimension>=MAX_ARRAY_DIMENSION)
		{
			printErrorMsg(list->parent, EXCESSIVE_ARRAY_DIM_DECLARATION);
			idNode->dataType = ERROR_TYPE;
			break;
		}
		processExprRelatedNode(dim);
		if(dim->dataType == ERROR_TYPE)
		{
			idNode->dataType = ERROR_TYPE;
		}
		else if(dim->dataType == FLOAT_TYPE)
		{
			printErrorMsg(dim->parent, ARRAY_SIZE_NOT_INT);
			idNode->dataType = ERROR_TYPE;
		}
		else if((dim->semantic_value.exprSemanticValue.isConstEval)&&(dim->semantic_value.exprSemanticValue.constEvalValue.iValue<0))
		{
			printErrorMsg(dim->parent, ARRAY_SIZE_NEGATIVE);
			idNode->dataType = ERROR_TYPE;
		}
		else
		{
			typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension] = dim->semantic_value.exprSemanticValue.constEvalValue.iValue;
		}
		dimension++;
		dim = dim->rightSibling;
	}
	typeDescriptor->properties.arrayProperties.dimension = dimension;
}

void declareFunction(AST_NODE* declarationNode)
{
	AST_NODE* id = declarationNode->child->rightSibling;
	AST_NODE* para = declarationNode->child->rightSibling->rightSibling->child;
	AST_NODE* block = declarationNode->child->rightSibling->rightSibling->rightSibling->rightSibling;

	processTypeNode(declarationNode->child);
	SymbolTableEntry* name;
	SymbolAttribute* attr;
	Parameter* current= NULL;
	Parameter* pre= NULL;
	Parameter* head= NULL;
	int count = 0;

	name = retrieveSymbol(id->semantic_value.identifierSemanticValue.identifierName);
	while(name != NULL)
	{
		if(name->attribute->attributeKind == FUNCTION_SIGNATURE)
		{
			break;
		}
		name = name->sameNameInOuterLevel;
	}
	if(name!=NULL)
	{
		printErrorMsgSpecial(id, id->semantic_value.identifierSemanticValue.identifierName, SYMBOL_REDECLARE);
		return;
	}

	attr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
	attr->attributeKind = FUNCTION_SIGNATURE;
	openScope();
	while(para != NULL)
	{
		count++;
		processDeclarationNode(para);
		current= (Parameter*)malloc(sizeof(Parameter));
		if(head == NULL)
		{
			head = current;
		}
		else
		{
			pre->next = current;
		}
		current->parameterName = para->child->rightSibling->semantic_value.identifierSemanticValue.identifierName;
		current->next = NULL;
		current->type = para->child->rightSibling->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
		para = para->rightSibling;
		pre = current;
		current = current->next;
	}
	attr->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
	attr->attr.functionSignature->parametersCount = count;
	attr->attr.functionSignature->parameterList = head;
	attr->attr.functionSignature->returnType = declarationNode->child->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;
	id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName,attr);

	processBlockNode(block);
	closeScope();
	id->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(id->semantic_value.identifierSemanticValue.identifierName,attr);
}
