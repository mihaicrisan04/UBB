import { Todo } from '@/types';

// Mock in-memory database
let todos: Todo[] = [
  { 
    id: '1', 
    title: 'Learn Next.js', 
    completed: false, 
    createdAt: new Date().toISOString() 
  },
  { 
    id: '2', 
    title: 'Build a Todo app', 
    completed: true, 
    createdAt: new Date().toISOString() 
  }
];

/**
 * Get all todos
 * @returns {Promise<Todo[]>} List of all todos
 */
export async function getAllTodos(): Promise<Todo[]> {
  // Simulate API latency
  await new Promise(resolve => setTimeout(resolve, 300));
  return [...todos];
}

/**
 * Get a todo by id
 * @param {string} id Todo id
 * @returns {Promise<Todo|null>} The todo or null if not found
 */
export async function getTodoById(id: string): Promise<Todo | null> {
  await new Promise(resolve => setTimeout(resolve, 200));
  return todos.find(todo => todo.id === id) || null;
}

/**
 * Create a new todo
 * @param {Omit<Todo, 'id' | 'createdAt'>} data Todo data
 * @returns {Promise<Todo>} The created todo
 */
export async function createTodo(data: Omit<Todo, 'id' | 'createdAt'>): Promise<Todo> {
  await new Promise(resolve => setTimeout(resolve, 300));
  
  const newTodo: Todo = {
    id: Date.now().toString(),
    ...data,
    createdAt: new Date().toISOString()
  };
  
  todos.push(newTodo);
  return newTodo;
}

/**
 * Update a todo
 * @param {string} id Todo id
 * @param {Partial<Omit<Todo, 'id' | 'createdAt'>>} data Updated todo data
 * @returns {Promise<Todo|null>} Updated todo or null if not found
 */
export async function updateTodo(
  id: string, 
  data: Partial<Omit<Todo, 'id' | 'createdAt'>>
): Promise<Todo | null> {
  await new Promise(resolve => setTimeout(resolve, 300));
  
  const index = todos.findIndex(todo => todo.id === id);
  if (index === -1) return null;
  
  todos[index] = {
    ...todos[index],
    ...data
  };
  
  return todos[index];
}

/**
 * Delete a todo
 * @param {string} id Todo id
 * @returns {Promise<boolean>} True if todo was deleted, false otherwise
 */
export async function deleteTodo(id: string): Promise<boolean> {
  await new Promise(resolve => setTimeout(resolve, 300));
  
  const initialLength = todos.length;
  todos = todos.filter(todo => todo.id !== id);
  
  return todos.length < initialLength;
}

/**
 * Skeleton function for future Supabase connection
 * Currently not used, but prepared for future database integration
 */
export async function initSupabaseClient() {
  // To be implemented when connecting to Supabase
  
  /*
  // Example Supabase client initialization:
  import { createClient } from '@supabase/supabase-js';
  
  const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL;
  const supabaseAnonKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY;
  
  if (!supabaseUrl || !supabaseAnonKey) {
    throw new Error('Missing Supabase environment variables');
  }
  
  return createClient(supabaseUrl, supabaseAnonKey);
  */
  
  return null;
}

/**
 * Skeleton function for future database operations with Supabase
 * Example of how todos would be fetched from Supabase
 */
export async function getTodosFromSupabase() {
  /*
  const supabase = await initSupabaseClient();
  const { data, error } = await supabase
    .from('todos')
    .select('*')
    .order('created_at', { ascending: false });
    
  if (error) {
    throw new Error(`Error fetching todos: ${error.message}`);
  }
  
  return data;
  */
  
  // Currently return mock data
  return await getAllTodos();
}

/**
 * Skeleton function for adding a todo to Supabase
 */
export async function addTodoToSupabase(title: string) {
  /*
  const supabase = await initSupabaseClient();
  const { data, error } = await supabase
    .from('todos')
    .insert([
      { 
        title, 
        completed: false 
      }
    ])
    .select();
    
  if (error) {
    throw new Error(`Error adding todo: ${error.message}`);
  }
  
  return data?.[0];
  */
  
  // Currently use mock data
  return await createTodo({ title, completed: false });
}
