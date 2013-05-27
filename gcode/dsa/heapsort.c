// heap sort
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct min_heap {
    int *data;
    size_t n;
    size_t max;
};

int parent_of(int child)
{
    return (child - 1) / 2;
}

int smaller_child_of(int parent)
{
    return parent * 2 + 1;
}

#define debug(fmt, ...) do {                    \
        fprintf(stderr,                         \
                "%s:%d: %s(): " fmt "\n",        \
                strrchr(__FILE__, '/')          \
                ? strrchr(__FILE__, '/') + 1    \
                : __FILE__,                     \
                __LINE__, __FUNCTION__,         \
                ##__VA_ARGS__);                 \
        fflush(stderr);                         \
    } while(0)

#define envdebug(fmt, ...) do {                                         \
        if (getenv("DEBUG") && strcmp(getenv("DEBUG"), "true") == 0) {  \
            fprintf(stderr,                                             \
                    "%s %s() %d: " fmt "\n",                            \
                    strrchr(__FILE__, '/')                              \
                    ? strrchr(__FILE__, '/') + 1                        \
                    : __FILE__,                                         \
                    __FUNCTION__, __LINE__,                             \
                    ##__VA_ARGS__);                                     \
            fflush(stderr);                                             \
        }                                                               \
    } while(0)

#define ddebug(a) do {                          \
        debug(#a " is %d", (a));                \
    } while(0)

#define xdebug(a) do {                          \
        debug(#a " is 0x%x", (a));              \
    } while(0)

#define die(fmt, ...) do {                      \
        debug(fmt, ##__VA_ARGS__);              \
        exit(-1);                               \
    } while(0)

#define ASSERT_EQUAL(a,b) do {                                  \
        if ((a) != (b)) {                                       \
            debug("error: %s is not equal to %s\n ", #a,  #b);  \
            exit(-1);                                           \
        } else {                                                \
            debug("OK: %s is equal to %s\n", #a, #b);           \
        }                                                       \
    } while (0)

#define die_if(cond) do {                                               \
        if (cond) {                                                     \
            debug("error: condition %s is true, need to die\n", #cond); \
            exit(-1);                                                   \
        }                                                               \
    } while (0)

#define ASSERT_NOT_EQUAL(a,b) do {                              \
        if ((a) == (b)) {                                       \
            debug("error: %s is equal to %s\n", #a, #b);        \
            exit(-1);                                           \
        } else {                                                \
            debug("OK: %s is not equal to %s\n", #a, #b);       \
        }                                                       \
    } while (0)

#define swap(a, b) do {                         \
              typeof(a) c = a;                  \
              a = b;                            \
              b = c;                            \
          } while (0)

void bubble_up(struct min_heap* heap, int node)
{
    if (node == 0) {
        return;
    }

    if (heap->data[parent_of(node)] <= heap->data[node]) {
        return;
    }

    swap(heap->data[parent_of(node)], heap->data[node]);
    bubble_up(heap, parent_of(node));
}

void insert_heap(struct min_heap* heap, int el)
{
    die_if (heap->n > heap->max);

    heap->data[heap->n++] = el;
    bubble_up(heap, heap->n-1);
}

void bubble_down(struct min_heap* heap, int node)
{
    int c0 = smaller_child_of(node);
    int c1 = c0 + 1;

    if (c0 > heap->n) {
        return;
    }

    if (c1 > heap->n || heap->data[c0] < heap->data[c1]) {
        if (heap->data[node] > heap->data[c0]) {
            swap(heap->data[node], heap->data[c0]);
            return bubble_down(heap, c0);
        }
    }

    if (c1 <= heap->n && heap->data[node] > heap->data[c1]) {
        swap(heap->data[node], heap->data[c1]);
        return bubble_down(heap, c1);
    }
}

int extract_min(struct min_heap* heap)
{
    die_if(heap->n == 0);
    int ret = heap->data[0];

    heap->data[0] = heap->data[--heap->n];

    bubble_down(heap, 0);
    return ret;
}
struct min_heap* make_heap(int *arr, size_t n)
{
    struct min_heap *heap = (struct min_heap *)malloc(sizeof (struct min_heap));
    heap->data = (int *)malloc(sizeof(int) * n);
    heap->n = 0;
    heap->max = n;

    int i = 0;
    while (i < n) {
        insert_heap(heap, arr[i]);
        i++;
    }
    return heap;
}


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

    int array[100];
    int i = 0;
    while (i < sizeof array / sizeof array[0]) {
        array[i] = random();
        printf("%d\n", array[i]);
        i++;
    }

    struct min_heap *heap = make_heap(array, sizeof array / sizeof array[0]);

    printf("hello\n");
    while (heap->n) {
        printf("%d\n", extract_min(heap));
    }
}
