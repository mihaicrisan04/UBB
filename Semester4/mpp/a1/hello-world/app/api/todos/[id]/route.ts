import { NextRequest, NextResponse } from 'next/server';
import { getTodoById, updateTodo, deleteTodo } from '@/lib/db';

/**
 * GET /api/todos/[id]
 * Retrieves a specific todo by id
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const todo = await getTodoById(params.id);
    
    if (!todo) {
      return NextResponse.json(
        { error: 'Todo not found' },
        { status: 404 }
      );
    }
    
    return NextResponse.json(todo);
  } catch (error) {
    console.error(`Failed to get todo ${params.id}:`, error);
    return NextResponse.json(
      { error: 'Failed to get todo' },
      { status: 500 }
    );
  }
}

/**
 * PATCH /api/todos/[id]
 * Updates a specific todo by id
 */
export async function PATCH(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const body = await request.json();
    const updatedTodo = await updateTodo(params.id, body);
    
    if (!updatedTodo) {
      return NextResponse.json(
        { error: 'Todo not found' },
        { status: 404 }
      );
    }
    
    return NextResponse.json(updatedTodo);
  } catch (error) {
    console.error(`Failed to update todo ${params.id}:`, error);
    return NextResponse.json(
      { error: 'Failed to update todo' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/todos/[id]
 * Deletes a specific todo by id
 */
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const success = await deleteTodo(params.id);
    
    if (!success) {
      return NextResponse.json(
        { error: 'Todo not found' },
        { status: 404 }
      );
    }
    
    return NextResponse.json({ success: true });
  } catch (error) {
    console.error(`Failed to delete todo ${params.id}:`, error);
    return NextResponse.json(
      { error: 'Failed to delete todo' },
      { status: 500 }
    );
  }
}
