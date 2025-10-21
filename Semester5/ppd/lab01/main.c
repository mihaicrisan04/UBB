#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>

#define MAX_WAREHOUSES 10
#define MAX_PRODUCTS_PER_WAREHOUSE 50
#define MAX_PRODUCT_NAME 50
#define NUM_THREADS 20
#define NUM_MOVES 1000
#define CONSITENCY_CHECK_INTERVAL 1

typedef struct {
  char name[MAX_PRODUCT_NAME];
  int quantity;
  pthread_mutex_t lock;
} Product;

typedef struct {
  int id;
  Product products[MAX_PRODUCTS_PER_WAREHOUSE];
  int product_count;
  pthread_mutex_t warehouse_lock;
} Warehouse;

typedef struct {
  int source_warehouse_id;
  int dest_warehouse_id;
  char product_name[MAX_PRODUCT_NAME];
  int quantity;
  int thread_id;
} MoveData;

typedef struct {
  char product_names[MAX_WAREHOUSES * MAX_PRODUCTS_PER_WAREHOUSE][MAX_PRODUCT_NAME];
  int total_quantities[MAX_WAREHOUSES * MAX_PRODUCTS_PER_WAREHOUSE];
  int product_count;
} ConsistencyData;

Warehouse warehouses[MAX_WAREHOUSES];
int warehouse_count = 0;

ConsistencyData consistency_data;
int should_continue_checking = 1; // flag to control the consistency checker thread

pthread_mutex_t global_mutex = PTHREAD_MUTEX_INITIALIZER;

Product* find_or_create_product(Warehouse* warehouse, const char* product_name) {
  // try to find an existing product (no locks needed for reading)
  for (int i = 0; i < warehouse->product_count; i++) {
    if (strcmp(warehouse->products[i].name, product_name) == 0) {
      return &warehouse->products[i];
    }
  }

  // lock the warehouse to modify it safely
  pthread_mutex_lock(&warehouse->warehouse_lock);

  // another thread might have added it while waiting for the lock (small chance i think -- suggested by ai)
  for (int i = 0; i < warehouse->product_count; i++) {
    if (strcmp(warehouse->products[i].name, product_name) == 0) {
      pthread_mutex_unlock(&warehouse->warehouse_lock);
      return &warehouse->products[i];
    }
  }

  // create a new product
  if (warehouse->product_count < MAX_PRODUCTS_PER_WAREHOUSE) {
    Product* new_product = &warehouse->products[warehouse->product_count];
    strcpy(new_product->name, product_name);
    new_product->quantity = 0;
    pthread_mutex_init(&new_product->lock, NULL);
    warehouse->product_count++;
    pthread_mutex_unlock(&warehouse->warehouse_lock);
    return new_product;
  }

  pthread_mutex_unlock(&warehouse->warehouse_lock);
  return NULL;
}

void init_warehouse(int warehouse_id) {
  if (warehouse_id >= MAX_WAREHOUSES) return;

  Warehouse* w = &warehouses[warehouse_count];
  w->id = warehouse_id;
  w->product_count = 0;
  pthread_mutex_init(&w->warehouse_lock, NULL);
  warehouse_count++;
}

// add a product to a warehouse (initial setup)
void add_product_to_warehouse(int warehouse_id, const char* product_name, int quantity) {
  for (int i = 0; i < warehouse_count; i++) {
    if (warehouses[i].id == warehouse_id) {
      Product* product = find_or_create_product(&warehouses[i], product_name);

      if (product) {
        pthread_mutex_lock(&product->lock);
        product->quantity += quantity;
        pthread_mutex_unlock(&product->lock);
        printf("Added %d units of %s to warehouse %d\n", quantity, product_name, warehouse_id);
      }

      // add to consistency data (no need for locking as the initial setup is made in the main thread before starting the others)
      int found = 0;
      for (int i = 0; i < consistency_data.product_count; i++) {
        if (strcmp(consistency_data.product_names[i], product_name) == 0) {
          consistency_data.total_quantities[i] += quantity;
          found = 1;
          break;
        }
      }
      if (!found) {
        strcpy(consistency_data.product_names[consistency_data.product_count], product->name);
        consistency_data.total_quantities[consistency_data.product_count] = quantity;
        consistency_data.product_count += 1;
      }

      break;
    }
  }
}

int move_product(int source_id, int dest_id, const char* product_name, int quantity) {
  Warehouse* source_warehouse = NULL;
  Warehouse* dest_warehouse = NULL;

  for (int i = 0; i < warehouse_count; i++) {
    if (warehouses[i].id == source_id) {
      source_warehouse = &warehouses[i];
    }
    if (warehouses[i].id == dest_id) {
      dest_warehouse = &warehouses[i];
    }
  }

  if (!source_warehouse || !dest_warehouse) {
    printf("Error: Warehouse not found\n");
    return -1;
  }

  Product* source_product = find_or_create_product(source_warehouse, product_name);
  Product* dest_product = find_or_create_product(dest_warehouse, product_name);

  if (!source_product || !dest_product) {
    printf("Error: Could not access products\n");
    return -1;
  }

  // lock prodcuts in consistent order to avoid deadlocks
  Product* first_lock = (source_product < dest_product) ? source_product : dest_product;
  Product* second_lock = (source_product < dest_product) ? dest_product : source_product;

  // lock the products before transfer
  pthread_mutex_lock(&first_lock->lock);
  if (first_lock != second_lock) {
    pthread_mutex_lock(&second_lock->lock);
  }

  if (source_product->quantity >= quantity) {
    source_product->quantity -= quantity;
    dest_product->quantity += quantity;

    printf("Moved %d units of %s from warehouse %d to warehouse %d\n", quantity, product_name, source_id, dest_id);

    if (first_lock != second_lock) {
      pthread_mutex_unlock(&second_lock->lock);
    }
    pthread_mutex_unlock(&first_lock->lock);
      return 0;
  } else {
    printf("Error: Not enough %s in warehouse %d (has %d, requested %d)\n", product_name, source_id, source_product->quantity, quantity);

    if (first_lock != second_lock) {
      pthread_mutex_unlock(&second_lock->lock);
    }
    pthread_mutex_unlock(&first_lock->lock);
    return -1;
  }
}

void inventory_check() {
  printf("\nInventory Check:\n");
  int total_inconsistencies = 0;

  // check the the inventory for a each unique product in all warehouses
  for (int u = 0; u < consistency_data.product_count; u++) {
      int total_quantity = 0;
      const char* product_name = consistency_data.product_names[u];

      // printf("Product: %s\n", product_name);

      // Check each warehouse for this product
      for (int w = 0; w < warehouse_count; w++) {
          for (int p = 0; p < warehouses[w].product_count; p++) {
              if (strcmp(warehouses[w].products[p].name, product_name) == 0) {
                  // Lock the product to get consistent read
                  pthread_mutex_lock(&warehouses[w].products[p].lock);
                  int quantity = warehouses[w].products[p].quantity;
                  pthread_mutex_unlock(&warehouses[w].products[p].lock);

                  // printf("  Warehouse %d: %d units\n", warehouses[w].id, quantity);
                  total_quantity += quantity;
              }
          }
      }

      printf("  Current total: %d\n", total_quantity);
      if (consistency_data.total_quantities[u] != total_quantity) {
          printf("  CONSISTENCY ERROR: Product %s (Expected: %d, Found: %d)\n",
                 product_name, consistency_data.total_quantities[u], total_quantity);
          total_inconsistencies++;
      }
  }
  printf("\nTotal inconsistencies found: %d\n", total_inconsistencies);
}

void *move_handler(void *arg) {
  MoveData* move_data = (MoveData*) arg;
  move_product(move_data->source_warehouse_id, move_data->dest_warehouse_id, move_data->product_name, move_data->quantity);

  free(move_data);
  return NULL;
}

void* consistency_checker(void* arg) {
  while (should_continue_checking) {
    printf("\033[2J\033[H");

    time_t now;
    time(&now);
    printf("Consistency checker at %s", ctime(&now));

    inventory_check();

    sleep(CONSITENCY_CHECK_INTERVAL);
  }
  return NULL;
}

int main(int argc, char *argv[]) {
  srand(time(NULL));

  init_warehouse(1);
  init_warehouse(2);

  add_product_to_warehouse(1, "Apples", 1000);
  add_product_to_warehouse(1, "Bananas", 1000);
  add_product_to_warehouse(1, "Oranges", 1000);
  add_product_to_warehouse(1, "Grapes", 1000);

  add_product_to_warehouse(2, "Apples", 1000);
  add_product_to_warehouse(2, "Bananas", 1000);
  add_product_to_warehouse(2, "Oranges", 1000);
  add_product_to_warehouse(2, "Grapes", 1000);

  // create the consistency checker thread
  pthread_t  consistency_thread;
  pthread_create(&consistency_thread, NULL, consistency_checker, NULL);

  // create worker threads
  pthread_t threads[NUM_THREADS];

  for (int i = 0; i < NUM_THREADS; i++) {
    MoveData* move_data = malloc(sizeof(MoveData));
    move_data->thread_id = i;

    // Generate random move parameters
    move_data->source_warehouse_id = (rand() % 2) + 1;  // 1 or 2
    move_data->dest_warehouse_id = (move_data->source_warehouse_id == 1) ? 2 : 1;

    const char* products[] = {"Apples", "Bananas", "Oranges", "Grapes"};
    strcpy(move_data->product_name, products[rand() % 4]);
    move_data->quantity = (rand() % 10) + 1; // 1-10 units

    pthread_create(&threads[i], NULL, move_handler, move_data);

    // sleep(1); // delay between the moves
    usleep(100000); // 0.1 second
  }

  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }

  // stop the consistecy checker thread
  should_continue_checking = 0;
  pthread_join(consistency_thread, NULL);

  for (int w = 0; w < warehouse_count; w++) {
    for (int p = 0; p < warehouses[w].product_count; p++) {
      pthread_mutex_destroy(&warehouses[w].products[p].lock);
    }
    pthread_mutex_destroy(&warehouses[w].warehouse_lock);
  }
  pthread_mutex_destroy(&global_mutex);

  return 0;
}
