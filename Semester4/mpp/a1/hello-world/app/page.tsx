"use client";

import { useState, useEffect } from 'react';
import { Todo } from '@/types';

export default function Home() {
  const [todos, setTodos] = useState<Todo[]>([]);
  const [newTodo, setNewTodo] = useState('');
  const [loading, setLoading] = useState(true);

  // Fetch todos on component mount
  useEffect(() => {
    fetchTodos();
  }, []);

  // Fetch all todos from the API
  const fetchTodos = async () => {
    try {
      setLoading(true);
      const response = await fetch('/api/todos');
      const data = await response.json();
      setTodos(data);
    } catch (error) {
      console.error('Failed to fetch todos:', error);
    } finally {
      setLoading(false);
    }
  };

  // Add a new todo
  const addTodo = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!newTodo.trim()) return;
    
    try {
      const response = await fetch('/api/todos', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ title: newTodo }),
      });
      
      if (response.ok) {
        setNewTodo('');
        fetchTodos(); // Refresh the list
      }
    } catch (error) {
      console.error('Failed to add todo:', error);
    }
  };

  // Toggle todo completion status
  const toggleTodo = async (id: string) => {
    try {
      const todoToUpdate = todos.find(todo => todo.id === id);
      if (!todoToUpdate) return;
      
      await fetch(`/api/todos/${id}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ completed: !todoToUpdate.completed }),
      });
      
      fetchTodos(); // Refresh the list
    } catch (error) {
      console.error('Failed to update todo:', error);
    }
  };
  
  // Delete a todo
  const deleteTodo = async (id: string) => {
    try {
      await fetch(`/api/todos/${id}`, {
        method: 'DELETE',
      });
      
      fetchTodos(); // Refresh the list
    } catch (error) {
      console.error('Failed to delete todo:', error);
    }
  };

  return (
    <div className="min-h-screen p-8">
      <div className="max-w-md mx-auto bg-white rounded-xl shadow-md overflow-hidden md:max-w-2xl">
        <div className="p-8">
          <h1 className="text-2xl font-bold mb-6 text-center">Todo List</h1>
          
          {/* Add Todo Form */}
          <form onSubmit={addTodo} className="mb-6">
            <div className="flex items-center border-b-2 border-gray-300 py-2">
              <input 
                className="appearance-none bg-transparent border-none w-full text-gray-700 mr-3 py-1 px-2 leading-tight focus:outline-none" 
                type="text" 
                placeholder="Add a new task..."
                value={newTodo}
                onChange={(e) => setNewTodo(e.target.value)}
              />
              <button 
                className="flex-shrink-0 bg-blue-500 hover:bg-blue-700 border-blue-500 hover:border-blue-700 text-sm border-4 text-white py-1 px-2 rounded" 
                type="submit"
              >
                Add
              </button>
            </div>
          </form>
          
          {/* Todo List */}
          <div className="space-y-3">
            {loading ? (
              <p className="text-center">Loading todos...</p>
            ) : todos.length === 0 ? (
              <p className="text-center text-gray-500">No todos yet. Add one above!</p>
            ) : (
              todos.map((todo) => (
                <div 
                  key={todo.id} 
                  className="flex items-center justify-between p-3 bg-gray-50 rounded-lg"
                >
                  <div className="flex items-center">
                    <input 
                      type="checkbox" 
                      checked={todo.completed} 
                      onChange={() => toggleTodo(todo.id)}
                      className="mr-3 h-4 w-4"
                    />
                    <span className={`${todo.completed ? 'line-through text-gray-400' : ''}`}>
                      {todo.title}
                    </span>
                  </div>
                  <button 
                    onClick={() => deleteTodo(todo.id)}
                    className="text-red-500 hover:text-red-700"
                  >
                    Delete
                  </button>
                </div>
              ))
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
