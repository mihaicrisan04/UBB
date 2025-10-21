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

typedef struct {
    int quantity;
    int moves_in;
    int moves_out;
} Warehouse;

typedef struct {
    int source_id;
    int dest_id;
    int thread_id;
} MoveData;

// Global variables
Warehouse warehouses[MAX_WAREHOUSES];
volatile int total_moves = 0;
volatile int should_continue = 1;
volatile int race_conditions_detected = 0;

// Track each operation for debugging
typedef struct {
    int thread_id;
    int source_id;
    int dest_id;
    int quantity;
    int source_before;
    int source_after;
    int dest_before;
    int dest_after;
    int total_before;
    int total_after;
} Operation;

#define MAX_OPERATIONS 1000
Operation operations[MAX_OPERATIONS];
volatile int operation_count = 0;

void clear_screen() {
    printf("\033[2J\033[H");
}

void log_operation(int thread_id, int source_id, int dest_id, int qty,
                  int src_before, int src_after, int dst_before, int dst_after) {
    if (operation_count < MAX_OPERATIONS) {
        Operation* op = &operations[operation_count++];
        op->thread_id = thread_id;
        op->source_id = source_id;
        op->dest_id = dest_id;
        op->quantity = qty;
        op->source_before = src_before;
        op->source_after = src_after;
        op->dest_before = dst_before;
        op->dest_after = dst_after;
        
        // Calculate totals
        int total_before = 0, total_after = 0;
        for (int i = 0; i < MAX_WAREHOUSES; i++) {
            if (i == source_id) {
                total_before += src_before;
                total_after += src_after;
            } else if (i == dest_id) {
                total_before += dst_before;
                total_after += dst_after;
            } else {
                total_before += warehouses[i].quantity;
                total_after += warehouses[i].quantity;
            }
        }
        op->total_before = total_before;
        op->total_after = total_after;
        
        if (total_before != total_after) {
            race_conditions_detected++;
        }
    }
}

void print_state() {
    clear_screen();
    printf("%s=== Warehouse Race Condition Demo ===%s\n\n", BLUE, RESET);
    
    int total = 0;
    printf("Current Warehouse States:\n");
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        total += warehouses[i].quantity;
        printf("Warehouse %d: %d units (In: %d, Out: %d)\n",
               i, warehouses[i].quantity, warehouses[i].moves_in, warehouses[i].moves_out);
    }
    
    printf("\nSystem Status:\n");
    printf("Total quantity: %d/%d\n", total, INITIAL_QUANTITY * MAX_WAREHOUSES);
    printf("Total moves: %d\n", total_moves);
    printf("Race conditions detected: %s%d%s\n", 
           race_conditions_detected > 0 ? RED : GREEN,
           race_conditions_detected,
           RESET);
    
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
            printf("  System total: %d -> %d ", 
                   op->total_before, op->total_after);
            
            if (op->total_before != op->total_after) {
                printf("%s(Lost %d units!)%s", RED,
                       op->total_before - op->total_after, RESET);
            }
            printf("\n");
        }
    }
}

void simulate_work() {
    usleep(rand() % 1000);  // 0-1ms delay
}

void move_products(int source_id, int dest_id, int thread_id) {
    Warehouse* source = &warehouses[source_id];
    Warehouse* dest = &warehouses[dest_id];
    
    // Read initial values
    int src_qty = source->quantity;
    int dst_qty = dest->quantity;
    
    if (src_qty >= MOVE_QUANTITY) {
        // Record state before modification
        int source_before = src_qty;
        int dest_before = dst_qty;
        
        simulate_work();  // Potential race condition point
        
        // Perform the move
        source->quantity = src_qty - MOVE_QUANTITY;
        source->moves_out++;
        
        simulate_work();  // Potential race condition point
        
        dest->quantity = dst_qty + MOVE_QUANTITY;
        dest->moves_in++;
        
        // Log the operation with before/after states
        log_operation(thread_id, source_id, dest_id, MOVE_QUANTITY,
                     source_before, source->quantity,
                     dest_before, dest->quantity);
        
        total_moves++;
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
    }
    
    printf("%s=== Warehouse Race Condition Demo ===%s\n\n", BLUE, RESET);
    printf("Initial setup:\n");
    printf("- Warehouses: %d\n", MAX_WAREHOUSES);
    printf("- Initial quantity per warehouse: %d\n", INITIAL_QUANTITY);
    printf("- Number of threads: %d\n", NUM_THREADS);
    printf("- Moves per thread: %d\n", MOVES_PER_THREAD);
    printf("- Units per move: %d\n\n", MOVE_QUANTITY);
    
    printf("Press Enter to start...");
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
    clear_screen();
    printf("\n%s=== Final Report ===%s\n\n", BLUE, RESET);
    
    int final_total = 0;
    printf("Final Warehouse States:\n");
    for (int i = 0; i < MAX_WAREHOUSES; i++) {
        printf("Warehouse %d: %d units (Moves in: %d, out: %d)\n",
               i, warehouses[i].quantity, warehouses[i].moves_in, warehouses[i].moves_out);
        final_total += warehouses[i].quantity;
    }
    
    printf("\nSystem Summary:\n");
    printf("- Total moves completed: %d\n", total_moves);
    printf("- Race conditions detected: %s%d%s\n",
           race_conditions_detected > 0 ? RED : GREEN,
           race_conditions_detected,
           RESET);
    printf("- Final system quantity: %d/%d ", 
           final_total, INITIAL_QUANTITY * MAX_WAREHOUSES);
    
    if (final_total != INITIAL_QUANTITY * MAX_WAREHOUSES) {
        printf("%s(Missing %d units)%s\n", RED,
               INITIAL_QUANTITY * MAX_WAREHOUSES - final_total,
               RESET);
    } else {
        printf("%s(OK)%s\n", GREEN, RESET);
    }
    
    printf("\nPress Enter to exit...");
    getchar();
    
    return 0;
}