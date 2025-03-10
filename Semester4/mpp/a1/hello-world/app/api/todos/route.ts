import { NextRequest, NextResponse } from 'next/server';
import { getAllTodos, createTodo } from '@/lib/db';

/**
 * GET /api/todos
 * Retrieves all todos
 */
export async function GET() {
  try {
    const todos = await getAllTodos();
    return NextResponse.json(todos);
  } catch (error) {
    console.error('Failed to get todos:', error);
    return NextResponse.json(
      { error: 'Failed to get todos' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/todos
 * Creates a new todo
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    
    // Validate input
    if (!body.title || typeof body.title !== 'string') {
      return NextResponse.json(
        { error: 'Title is required and must be a string' },
        { status: 400 }
      );
    }
    
    // Create new todo (defaults to not completed)
    const newTodo = await createTodo({
      title: body.title,
      completed: false
    });
    
    return NextResponse.json(newTodo, { status: 201 });
  } catch (error) {
    console.error('Failed to create todo:', error);
    return NextResponse.json(
      { error: 'Failed to create todo' },
      { status: 500 }
    );
  }
}
