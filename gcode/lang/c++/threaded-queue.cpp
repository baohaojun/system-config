/**** start of bhj auto includes ****/
#include <assert.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/**** end of bhj auto includes ****/


class video_frame {
public:
    int m_n;
    static int n;
    video_frame() {
        m_n = n++;
    }
};

int video_frame::n;
class thread_queue {
public:
    void enqueue(const video_frame& fr);
    const video_frame& dequeue();
    bool empty();
    bool is_full();

    size_t size();
    size_t capacity() {return m_capacity;}

    explicit thread_queue(size_t init_size);
    ~thread_queue();
    thread_queue(const thread_queue&) { 
        assert(0);
    };
    thread_queue operator=(const thread_queue&) {
        assert(0);
    }

private:
    const video_frame** m_storage;
    size_t m_capacity;
    size_t head, tail;
};

thread_queue::thread_queue(size_t init_size) :
    m_capacity(init_size), head(0), tail(0)
{
    if (!init_size) {
        m_capacity = init_size = 1;
    }
    m_storage = (const video_frame**)malloc(sizeof (const video_frame*) * init_size);
}

thread_queue::~thread_queue()
{
    free(m_storage);
}

bool thread_queue::empty()
{
    return head == tail;
}

size_t thread_queue::size()
{
    return (tail - head + capacity()) % capacity();
}

bool thread_queue::is_full()
{
    return size() == capacity() - 1;
}
void thread_queue::enqueue(const video_frame& fr)
{
    if (is_full()) {
        size_t new_cap = m_capacity * 1.2 + 5;
        const video_frame** new_storage = (const video_frame**)malloc(sizeof (const video_frame*) * new_cap);

        if (head < tail) {
            memcpy(new_storage, m_storage + head, sizeof (const video_frame*) * size());
        } else {
            memcpy(new_storage, m_storage + head, sizeof (const video_frame*) * (m_capacity - head));
            memcpy(new_storage + m_capacity - head, m_storage, sizeof (const video_frame*) * tail);
        }

        head = 0;
        tail = size();
        m_capacity = new_cap;
        free(m_storage);
        m_storage = new_storage;
    }

    m_storage[tail++] = &fr;
    if (tail == capacity()) {
        tail = 0;
    }
}

const video_frame& thread_queue::dequeue()
{
    assert(!empty());

    return *m_storage[head++];
    if (head == capacity()) {
        head = 0;
    }
}

int main()
{
    thread_queue *q = new thread_queue(5);

    for (int i = 0; i < 20; i++) {
        q->enqueue(* new video_frame());
    }

    const video_frame *frame = &q->dequeue();
    frame = &q->dequeue();
    printf("q's size is %d, frame is %d\n", q->size(), frame->m_n);
    thread_queue x(5);
    x.enqueue(* new video_frame());
    printf("q's size is %d, cap is %d\n", x.size(), x.capacity());
}
