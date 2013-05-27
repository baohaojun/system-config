// reverse single list
#include <stdlib.h>
#include <stdio.h>

struct list {
    int data;
    struct list* next;
};

void reverse_list(struct list** head)
{
    struct list* prev = NULL;
    while (*head) {
        struct list* next = (*head)->next;
        (*head)->next = prev;
        prev = *head;
        *head = next;
    }

    *head = prev;
}

void print_list(struct list* head)
{
    while (head) {
        printf("%d\n", head->data);
        head = head->next;
    }
}

void insert_list(struct list** head, int data)
{
    struct list* list= (struct list*)malloc(sizeof (struct list));
    list->data = data;
    list->next = *head;
    *head = list;
}

int main(int argc, char* argv[])
{
    int i;
    struct list* head = NULL;
    for (i = 0; i < 10; i++) {
        insert_list(&head, random());
    }

    print_list(head);
    reverse_list(&head);

    printf("reversed\n");
    print_list(head);
}
