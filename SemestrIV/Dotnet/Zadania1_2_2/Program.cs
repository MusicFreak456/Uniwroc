using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Zadanie1_2_2 {
    public class BinaryTreeNode<T> {
        public BinaryTreeNode<T> Left;
        public BinaryTreeNode<T> Right;
        public T Value;


        public class DFSEnumerator : IEnumerator, IEnumerable {
            private BinaryTreeNode<T> _parent;
            private BinaryTreeNode<T> _current;
            private Stack<BinaryTreeNode<T>> _stack;
            public DFSEnumerator(BinaryTreeNode<T> parent) {
                this._parent = parent;
                this._stack = new Stack<BinaryTreeNode<T>>();
                this.Reset();
            }

            public IEnumerator GetEnumerator() {
                return this;
            }

            public object Current {
                get {
                    this._current = this._stack.Pop();
                    return this._current.Value;
                }
            }

            public bool MoveNext() {
                BinaryTreeNode<T> top = this._current;
                if(top.Right != null) {
                    this._stack.Push(top.Right);
                }
                if(top.Left != null) {
                    this._stack.Push(top.Left);
                }
                if (this._stack.Count == 0) return false;
                return true;
            }
            public void Reset() {
                this._stack.Clear();
                this._current = new BinaryTreeNode<T>() {
                    Left = _parent
                };
            }

            public void Dispose() { }

        }

        public class BFSEnumerator : IEnumerator, IEnumerable {
            private BinaryTreeNode<T> _parent;
            private BinaryTreeNode<T> _current;
            private Queue<BinaryTreeNode<T>> _queue;
            public BFSEnumerator(BinaryTreeNode<T> parent) {
                this._parent = parent;
                this._queue = new Queue<BinaryTreeNode<T>>();
                this.Reset();
            }

            public IEnumerator GetEnumerator() {
                return this;
            }

            public object Current {
                get {
                    this._current = this._queue.Dequeue();
                    return this._current.Value;
                }
            }

            public bool MoveNext() {
                BinaryTreeNode<T> top = this._current;
                if (top.Right != null) {
                    this._queue.Enqueue(top.Right);
                }
                if (top.Left != null) {
                    this._queue.Enqueue(top.Left);
                }
                if (this._queue.Count == 0) return false;
                return true;
            }
            public void Reset() {
                this._queue.Clear();
                this._current = new BinaryTreeNode<T>() {
                    Left = _parent
                };
            }

            public void Dispose() { }

        }

        //DFS - preorder
        public IEnumerable<T> DFS() {
            yield return this.Value;
            if(this.Left != null) {
                foreach(T x in Left.DFS()) {
                    yield return x;
                }
            }
            if(this.Right != null) {
                foreach (T x in Right.DFS()) {
                    yield return x;
                }
            }
        }

        public IEnumerable<T> BFS() {
            Queue<BinaryTreeNode<T>> queue = new Queue<BinaryTreeNode<T>>();
            queue.Enqueue(this);
            while(queue.Count != 0) {
                BinaryTreeNode<T> current = queue.Dequeue();
                yield return current.Value;
                if(current.Left != null) {
                    queue.Enqueue(current.Left);
                }
                if(current.Right != null) {
                    queue.Enqueue(current.Right);
                }
            }
        }

        public IEnumerable NonYieldDFS() {
            return new DFSEnumerator(this);
        }

        public IEnumerable NonYieldBFS() {
            return new BFSEnumerator(this);
        }


    }
    class Program {
        static void Main(string[] args) {
            BinaryTreeNode<int> example = new BinaryTreeNode<int>() {
                Left = new BinaryTreeNode<int>() {
                    Value = 1
                },
                Right = new BinaryTreeNode<int>() {
                    Left = new BinaryTreeNode<int> {
                        Value = 3
                    },
                    Right = new BinaryTreeNode<int> {
                        Value = 5
                    },
                    Value = 4
                },
                Value = 2
            };
            Console.WriteLine("DFS yield (preorder)");
            foreach(int x in example.DFS()) {
                Console.Write(x.ToString() + " ");
            }
            Console.Write("\n");
            Console.WriteLine("DFS nonyield (preorder)");
            foreach (int x in example.NonYieldDFS()) {
                Console.Write(x.ToString() + " ");
            }
            Console.Write("\n");
            Console.WriteLine("BFS yield");
            foreach (int x in example.NonYieldBFS()) {
                Console.Write(x.ToString() + " ");
            }
            Console.Write("\n");
            Console.WriteLine("BFS nonyield");
            foreach (int x in example.NonYieldBFS()) {
                Console.Write(x.ToString() + " ");
            }
            Console.Write("\n");
        }
    }
}
