import java.util.ArrayDeque;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class ScalarProductJava {

  // Bounded buffer between producer and consumer
  static class BoundedQueue {

    private final ArrayDeque<Double> deque;
    private final int capacity;
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition notFull = lock.newCondition();
    private final Condition notEmpty = lock.newCondition();
    private boolean done = false; // set by producer when finished

    BoundedQueue(int capacity) {
      this.capacity = capacity;
      this.deque = new ArrayDeque<>(capacity);
    }

    public void put(double value) throws InterruptedException {
      lock.lock();
      try {
        while (deque.size() == capacity) {
          notFull.await();
        }
        deque.addLast(value);
        // Signal consumer that a new item is available
        notEmpty.signal();
      } finally {
        lock.unlock();
      }
    }

    public Double take() throws InterruptedException {
      lock.lock();
      try {
        while (deque.isEmpty() && !done) {
          notEmpty.await();
        }
        if (deque.isEmpty() && done) {
          return null; // no more items will arrive
        }
        Double v = deque.removeFirst();
        // Signal producer that space is available
        notFull.signal();
        return v;
      } finally {
        lock.unlock();
      }
    }

    public void setDone() {
      lock.lock();
      try {
        done = true;
        // Wake up consumer if waiting on empty
        notEmpty.signalAll();
      } finally {
        lock.unlock();
      }
    }
  }

  public static void main(String[] args) throws Exception {
    int n = 100; // vector length
    int queueSize = 5; // bounded queue capacity
    if (args.length >= 1) n = Integer.parseInt(args[0]);
    if (args.length >= 2) queueSize = Integer.parseInt(args[1]);

    double[] a = new double[n];
    double[] b = new double[n];
    for (int i = 0; i < n; i++) {
      a[i] = (double) (i + 1);
      b[i] = (double) (i + 1);
    }

    BoundedQueue buffer = new BoundedQueue(queueSize);
    final double[] result = new double[1];

    Thread producer = new Thread(
      () -> {
        try {
          for (int i = 0; i < a.length; i++) {
            buffer.put(a[i] * b[i]);
          }
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        } finally {
          buffer.setDone();
        }
      },
      "producer"
    );

    Thread consumer = new Thread(
      () -> {
        double sum = 0.0;
        try {
          while (true) {
            Double v = buffer.take();
            if (v == null) break; // finished
            sum += v;
          }
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
        result[0] = sum;
      },
      "consumer"
    );

    producer.start();
    consumer.start();
    producer.join();
    consumer.join();

    System.out.println("n=" + n + ", queueSize=" + queueSize);
    System.out.println("scalarProduct=" + result[0]);
  }
}
