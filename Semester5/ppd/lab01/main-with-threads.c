#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>

#define MAX_WAREHOUSES 4
#define NUM_THREADS 20
#define MOVES_PER_THREAD 10
#define MOVE_QUANTITY 100
#define INITIAL_QUANTITY 1000
#define REFRESH_RATE 500000  // 0.5 seconds

// ANSI Colors
#define RED     "\033[0;31m"
#define GREEN   "\033[0;32m"
#define YELLOW  "\033[0;33m"
#define BLUE    "\033[0;34m"
#define RESET   "\033[0m"
#define CLEAR   "\033[2J\033[H"

typedef struct {
    int quantity;
    int moves_in;
    int moves_out;
    pthread_mutex_t lock;
} Warehouse;

typedef struct {
    int source_id;
    int dest_id;
    int thread_id;
} MoveData;

typedef struct {
    int thread_id;
    int source_id;
    int dest_id;
    int quantity;
    int source_before;
    int source_after;
    int dest_before;
    int dest_after;
    int system_total;
    time_t timestamp;
} Operation;

// Global variables
Warehouse warehouses[MAX_WAREHOUSES];
volatile int total_moves = 0;
volatile int should_continue = 1;

#define MAX_OPERATIONS 1000
Operation operations[MAX_OPERATIONS];
volatile int operation_count = 0;
pthread_mutex_t operations_lock = PTHREAD_MUTEX_INITIALIZER;

void log_operation(Operation op) {
    pthread_mutex_lock(&operations_lock);
    if (operation_count < MAX_OPERATIONS) {
        operations[operation_count++] = op;
    }
    pthread_mutex_unlock(&operations_lock);
}

int get_system_total() {
    int total = 0;
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        pthread_mutex_lock(&warehouses[i].lock);
        total += warehouses[i].quantity;
        pthread_mutex_unlock(&warehouses[i].lock);
    }
    return total;
}

void print_state() {
    printf(CLEAR);
    printf("%s=== Warehouse Management System (Thread-Safe Version) ===%s\n\n", 
           BLUE, RESET);
    
    int total = 0;
    printf("Current Warehouse States:\n");
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        pthread_mutex_lock(&warehouses[i].lock);
        int qty = warehouses[i].quantity;
        int moves_in = warehouses[i].moves_in;
        int moves_out = warehouses[i].moves_out;
        total += qty;
        printf("Warehouse %d: %d units (In: %d, Out: %d)\n",
               i, qty, moves_in, moves_out);
        pthread_mutex_unlock(&warehouses[i].lock);
    }
    
    printf("\nSystem Status:\n");
    printf("Total quantity: %d/%d\n", total, INITIAL_QUANTITY * MAX_WAREHOUSES);
    printf("Total moves: %d\n", total_moves);
    
    pthread_mutex_lock(&operations_lock);
    if (operation_count > 0) {
        printf("\nLast 5 Operations:\n");
        int start = operation_count > 5 ? operation_count - 5 : 0;
        for (int i = start; i < operation_count; i++) {
            Operation* op = &operations[i];
            printf("\nThread %d: W%d -> W%d (%d units)\n",
                   op->thread_id, op->source_id, op->dest_id, op->quantity);
            printf("  Source W%d: %d -> %d\n", 
                   op->source_id, op->source_before, op->source_after);
            printf("  Dest   W%d: %d -> %d\n", 
                   op->dest_id, op->dest_before, op->dest_after);
            printf("  System total: %d\n", op->system_total);
        }
    }
    pthread_mutex_unlock(&operations_lock);
}

void simulate_work() {
    usleep(rand() % 1000);  // 0-1ms delay
}

void move_products(int source_id, int dest_id, int thread_id) {
    Warehouse* source = &warehouses[source_id];
    Warehouse* dest = &warehouses[dest_id];
    
    // Lock warehouses in a consistent order to prevent deadlocks
    if (source_id < dest_id) {
        pthread_mutex_lock(&source->lock);
        pthread_mutex_lock(&dest->lock);
    } else {
        pthread_mutex_lock(&dest->lock);
        pthread_mutex_lock(&source->lock);
    }
    
    // Read initial values
    int src_qty = source->quantity;
    int dst_qty = dest->quantity;
    
    if (src_qty >= MOVE_QUANTITY) {
        // Record state before modification
        Operation op = {
            .thread_id = thread_id,
            .source_id = source_id,
            .dest_id = dest_id,
            .quantity = MOVE_QUANTITY,
            .source_before = src_qty,
            .dest_before = dst_qty
        };
        
        // Perform the move
        source->quantity = src_qty - MOVE_QUANTITY;
        source->moves_out++;
        dest->quantity = dst_qty + MOVE_QUANTITY;
        dest->moves_in++;
        
        // Record after states
        op.source_after = source->quantity;
        op.dest_after = dest->quantity;
        
        // Release locks in reverse order
        if (source_id < dest_id) {
            pthread_mutex_unlock(&dest->lock);
            pthread_mutex_unlock(&source->lock);
        } else {
            pthread_mutex_unlock(&source->lock);
            pthread_mutex_unlock(&dest->lock);
        }
        
        // Get system total after the move (with separate locks)
        op.system_total = get_system_total();
        op.timestamp = time(NULL);
        
        // Log the operation
        log_operation(op);
        total_moves++;
    } else {
        // Release locks if no move was performed
        if (source_id < dest_id) {
            pthread_mutex_unlock(&dest->lock);
            pthread_mutex_unlock(&source->lock);
        } else {
            pthread_mutex_unlock(&source->lock);
            pthread_mutex_unlock(&dest->lock);
        }
    }
}

void* move_handler(void* arg) {
    MoveData* data = (MoveData*)arg;
    
    for (int i = 0; i < MOVES_PER_THREAD; i++) {
        move_products(data->source_id, data->dest_id, data->thread_id);
        usleep(rand() % 5000);  // 0-5ms delay between moves
    }
    
    free(data);
    return NULL;
}

void* monitor_handler(void* arg) {
    while (should_continue) {
        print_state();
        usleep(REFRESH_RATE);
    }
    return NULL;
}

int main() {
    srand(time(NULL));
    
    // Initialize warehouses
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        warehouses[i].quantity = INITIAL_QUANTITY;
        warehouses[i].moves_in = 0;
        warehouses[i].moves_out = 0;
        pthread_mutex_init(&warehouses[i].lock, NULL);
    }
    
    printf("%s=== Warehouse Management System (Thread-Safe Version) ===%s\n\n", 
           BLUE, RESET);
    printf("Initial setup:\n");
    printf("- Warehouses: %d\n", MAX_WAREHOUSES);
    printf("- Initial quantity per warehouse: %d\n", INITIAL_QUANTITY);
    printf("- Number of threads: %d\n", NUM_THREADS);
    printf("- Moves per thread: %d\n", MOVES_PER_THREAD);
    printf("- Units per move: %d\n\n", MOVE_QUANTITY);
    
    printf("Press Enter to start simulation...");
    getchar();
    
    // Start monitor thread
    pthread_t monitor_thread;
    pthread_create(&monitor_thread, NULL, monitor_handler, NULL);
    
    // Create worker threads
    pthread_t threads[NUM_THREADS];
    
    for (int i = 0; i < NUM_THREADS; i++) {
        MoveData* data = malloc(sizeof(MoveData));
        data->thread_id = i;
        data->source_id = rand() % MAX_WAREHOUSES;
        do {
            data->dest_id = rand() % MAX_WAREHOUSES;
        } while (data->dest_id == data->source_id);
        
        pthread_create(&threads[i], NULL, move_handler, data);
    }
    
    // Wait for worker threads
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }
    
    // Stop and wait for monitor
    should_continue = 0;
    pthread_join(monitor_thread, NULL);
    
    // Final report
    printf(CLEAR);
    printf("\n%s=== Final Report ===%s\n\n", BLUE, RESET);
    
    int final_total = 0;
    printf("Final Warehouse States:\n");
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        pthread_mutex_lock(&warehouses[i].lock);
        int qty = warehouses[i].quantity;
        int moves_in = warehouses[i].moves_in;
        int moves_out = warehouses[i].moves_out;
        final_total += qty;
        printf("Warehouse %d: %d units (Moves in: %d, out: %d)\n",
               i, qty, moves_in, moves_out);
        pthread_mutex_unlock(&warehouses[i].lock);
    }
    
    printf("\nSystem Summary:\n");
    printf("- Total moves completed: %d\n", total_moves);
    printf("- Final system quantity: %d/%d ", 
           final_total, INITIAL_QUANTITY * MAX_WAREHOUSES);
    
    if (final_total == INITIAL_QUANTITY * MAX_WAREHOUSES) {
        printf("%s(OK)%s\n", GREEN, RESET);
    } else {
        printf("%s(ERROR: Missing %d units)%s\n", 
               RED, INITIAL_QUANTITY * MAX_WAREHOUSES - final_total, RESET);
    }
    
    // Cleanup
    pthread_mutex_destroy(&operations_lock);
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        pthread_mutex_destroy(&warehouses[i].lock);
    }
    
    printf("\nPress Enter to exit...");
    getchar();
    
    return 0;
}