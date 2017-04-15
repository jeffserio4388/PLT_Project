/*
 * This method is a set of tools used to implement a singly linked list data 
 * structure as per the lab 3 assignment. details about each function 
 * is located in the comments below
 */
#include "mylist.h"
#include <stdio.h>
#include <stdlib.h>

/*
 * This function creates a pointer to a struct Node, and mallocs the
 * proper space for the Node. it then assigns the list(from the 
 * formal parameters) pointer's current head to the node's next variable,
 * then assigns the data(from parameters) to the node's data variable,
 * then returns node to the calling function.
 */
struct Node *addFront(struct List *list, void *data){
    struct Node *node = malloc(sizeof(struct Node));
    node -> next = list->head;
    node -> data = data;
    list->head = node;
    return node;
}

/*
 * Creates a node pointer of the provided list's head, then uses
 * a while loop to iterat through the loop, using the provided 
 * function pointer to print each element, then assign the node's
 * next pointer to node, then it terminates when it reaches the node with
 * the value of 0, which is the tail node of the list structure.
 */
void traverseList (struct List *list, void (*f)(void *)){
    struct Node *node = list->head;
    while(node){
        f(node->data);
        node = node->next;
    }
}

/*
 * It takes the void * data provided by the caller, then casts it to a 
 * double *, dereferences it then multiplies it by -1 and then puts 
 * that value into the callers variable. 
 */ 
void flipSignDouble(void *data){
    *(double *)data *= -1.0;
}

/*
 * Casts the provided const void * variables as double *, then 
 * dereferences them, and compares for equality. if they are
 * equal, then it returns a 0, if they are not equal it returns
 * a 1, as per the instructions.
 */
int compareDouble(const void *data1, const void *data2){
    return (*(double *)data1 == *(double *)data2) ? 0 : 1 ;
}

/*
 * This function iterates through a list, creates a node pointer and 
 * sets it to be the list's head, then it compares the data of
 * each node to the dataSought variable, using the provided
 * compar function. If there is a match the compar function returns 0 and
 * stops the loop, but if not it asigns node->next to node, and 
 * iterates. It searches one by one until it finds a match or reaches 
 * the end of the list. It then returns the node variable to the
 * caller
 */
struct Node *findNode(struct List *list, const void *dataSought, 
        int (*compar)(const void *, const void *)){
    struct Node *node = list->head;
    while(node && compar(dataSought, node->data)){
        node = node->next;
    }
    return (node) ? node : NULL;
}

/*
 * This method takes the head->data and head pointers from the 
 * list parameter, then copys them to temporary variables. it
 * then asigns the curent list head to the next variable of its
 * head, frees the previous head, and returns the data copied from
 * the head to the caller.
 */
void *popFront(struct List *list){
    //checks if the head node is 0, which means an empty list, and
    //if it is it returns NULL
    if(!list->head)
        return NULL;
    void *data = list->head->data;
    struct Node*toPop = list->head;
    list->head = list->head->next;
    free(toPop);
    return data;
}

/*
 * This function continuously calls popFront, using the given list as 
 * the argument, untill the function returns NULL, meaning the list is
 * empty. It does this using a while loop the acts as a driver that
 * iterates while poping each node untill the popFront function
 * returns a NULL value and the list is empty.
 */
void removeAllNodes(struct List *list){
    while(popFront(list) != NULL){};
}

/*
 * This function first checks if the prevNode is NULL, meaning the 
 * list is empty, and if it is it calls addFront to create a node 
 * and add it to the list. Then it creates a new node, by creating a 
 * struct Node pointer and mallocing the right amount of data, puts the
 * given data, from the parameter, into the new node. It then sets
 * its next to the next of the prevNode variable, and then sets itself 
 * to the next of the prevNode, to add it in front of it. after all
 * that it returns the node to the caller.
 */
struct Node *addAfter(struct List *list,
        struct Node *prevNode, void *data){
    if(prevNode == NULL)
        return addFront(list, data);
    struct Node *node = malloc(sizeof(struct Node));
    node->next = prevNode->next;
    node->data = data;
    prevNode->next = node;

    return node;

}

/*
 * This function uses post-order transversal recursion to reverse the given
 * list. It fisrt checks if the list is empty with the conditional that
 * is true if the current head of the list is not 0, because and empty
 * list has a head pointer of 0. then if it is not empty it checks
 * for the base case, using the inner conditional if. If it the list
 * is empty, or if the base case has been reached, it does nothing.
 * The base case is when the last node in the list before the 0 tail node
 * is the head node. If it is not the base case, it creates a Node pointer
 * called currentNode as a place holder, so that the node is not lost
 * in the next step of changing the head of the list to the next pointer
 * of the currentNode. It then calls reverseList(list) and it repeats 
 * until it reaches the aforementioned base case. After it has reached 
 * the base case, the function sets the next pointer of currentNode's next
 * pointer to itself, and its own next to 0 recursively, for each call to
 * the function, in post-order. the original first node->next stays at 0
 * so that it becomes the new end of the list. This effectively flips all
 * of the pointers and reverses the list using post-order transversal
 */
void reverseList(struct List *list){
    
    if(list->head){
        if(list->head->next){
            struct Node *currentNode = list->head;
            list->head = list->head->next;
//uses post-order transversal so that it traverses the list and then 
//reverses the pointers.
            reverseList(list);
            currentNode->next->next = currentNode;
            currentNode->next = 0;
        }
    }
}
