/**
 * General implementation of AVL tree (no duplicate keys).
 * Instead of balance factor, height is stored.
 * It is easier to implement and gains from balance factors are marginal.
 * Class K (key) should have < and == operators.
 * Class V (value) should have:
 *  - function update(left, right), which takes two constant V pointers
 *    (might be NULL) and modifies itself based on them.
 */

template <class K, class V>
class AVLTree;

template <class K, class V>
class AVLNode {
    friend class AVLTree<K, V>;

    private:
        AVLNode<K, V> *left, *right, *parent;
        K key;
        int height;
    
        AVLNode(AVLNode *parent, const K &key, const V &value):
            left(nullptr), right(nullptr), parent(parent),
            key(key), value(value), height(1) {}
        ~AVLNode() {
            delete left;
            delete right;
        }
        
    public:
        K getKey() {
            return key;
        }
        V value;
        
        friend int getHeight(AVLNode<K, V> *node) {
            if (node == nullptr)
                return 0;
            else
                return node->height;
        }
        
        void update() {
            int h1 = getHeight(left) + 1,
                h2 = getHeight(right) + 1;
            height = h1 > h2 ? h1 : h2;
            const V *value1 = left ? &left->value : nullptr,
                    *value2 = right ? &right->value : nullptr;
            value.update(value1, value2);
        }
};

template <class K, class V>
class AVLTree {
    private:
        AVLNode<K, V> *root;
        
        void updateNode(AVLNode<K, V> *node);
        void rotateNodeRight(AVLNode<K, V> *node);
        void rotateNodeLeft(AVLNode<K, V> *node);
        void correctHeightRight(AVLNode<K, V> *node);
        void correctHeightLeft(AVLNode<K, V> *node);
        AVLNode<K, V> *getNextNode(AVLNode<K, V> *node);
    
    public:
        AVLTree(): root(nullptr) {}
        ~AVLTree() {
            delete root;
        }
        
        AVLNode<K, V> *search(const K &key);
        bool insert(const K &key, const V &value);
        bool remove(const K &key);
        bool empty() {
            return root == nullptr;
        }
        V getValueAtRoot() {
            return root->value;
        }
        void printTree() {
            printNode(root, 0);
        }
};

/**
 * Rotates node to the right (left side is heavier).
 * 
 * @param node
 */
template <class K, class V>
void AVLTree<K, V>::rotateNodeRight(AVLNode<K, V> *node) {
    auto *a = node,
         *b = node->left,
         *X = node->left->right,
         *Y = node->parent;
    a->parent = b;
    a->left = X;
    b->right = a;
    b->parent = Y;
    if (X != nullptr)
        X->parent = a;
    if (Y != nullptr) {
        if (Y->left == a)
            Y->left = b;
        else
            Y->right = b;
    } else // a was root
        root = b;
    a->update();
    b->update();
}


/**
 * Rotates node to the left (right side is heavier).
 * 
 * @param node
 */
template <class K, class V>
void AVLTree<K, V>::rotateNodeLeft(AVLNode<K, V> *node) {
    auto *a = node,
         *b = node->right,
         *X = node->right->left,
         *Y = node->parent;
    a->parent = b;
    a->right = X;
    b->parent = Y;
    b->left = a;
    if (X != nullptr)
        X->parent = a;
    if (Y != nullptr) {
        if (Y->left == a)
            Y->left = b;
        else
            Y->right = b;
    } else // a was root
        root = b;
    a->update();
    b->update();
}

/**
 * Corrects tree using rotations to the right (left side is heavier).
 * 
 * @param node
 */
template <class K, class V>
void AVLTree<K, V>::correctHeightRight(AVLNode<K, V> *node) {
    if (getHeight(node->left->left) > getHeight(node->left->right))
        rotateNodeRight(node);
    else {
        rotateNodeLeft(node->left);
        rotateNodeRight(node);
    }
}

/**
 * Corrects tree using rotations to the left (right side is heavier).
 * 
 * @param node
 */
template <class K, class V>
void AVLTree<K, V>::correctHeightLeft(AVLNode<K, V> *node) {
    if (getHeight(node->right->right) > getHeight(node->right->left))
        rotateNodeLeft(node);
    else {
        rotateNodeRight(node->right);
        rotateNodeLeft(node);
    }
}

/**
 * Updates node.
 * 
 * @param node
 */
template <class K, class V>
void AVLTree<K, V>::updateNode(AVLNode<K, V> *node) {
    if (node == nullptr)
        return;
    int h1 = getHeight(node->left),
        h2 = getHeight(node->right),
        balance = h1 - h2;
    if (balance == 2)
        correctHeightRight(node);
    else if (balance == -2)
        correctHeightLeft(node);
    else
        node->update();
    updateNode(node->parent);
}

/**
 * Get next inorder node.
 * 
 * @param node
 * @return next inorder node
 */
template <class K, class V>
AVLNode<K, V> *AVLTree<K, V>::getNextNode(AVLNode<K, V> *node) {
    if (node == nullptr)
        return nullptr;
    
    if (node->right != nullptr) {
        node = node->right;
        while (node->left != nullptr)
            node = node->left;
        return node;
    }
    
    while (node->parent != nullptr) {
        if (node->parent->left == node)
            return node->parent;
        node = node->parent;
    }
    
    return nullptr; // node was the last node in tree
}

/**
 * Returns pointer to AVLNode with the same key as \p key. 
 * To access value, use pointer->getValue();
 * If key is not present in tree, nullptr is returned.
 * 
 * @param key
 * @return pointer to node or nullptr
 */
template <class K, class V>
AVLNode<K, V> *AVLTree<K, V>::search(const K &key) {
    auto *pointer = root;
    while (pointer != nullptr) {
        if (pointer->key == key)
            return pointer;
        else if (pointer->key < key)
            pointer = pointer->right;
        else
            pointer = pointer->left;
    }
    return nullptr;
}

/**
 * Inserts key with value into tree, unless key already exists in tree.
 * 
 * @param key
 * @param value
 * @return Was node inserted?
 */
template <class K, class V>
bool AVLTree<K, V>::insert(const K &key, const V &value) {
    auto *pointer = root;
    if (pointer == nullptr) { // Tree is empty
        root = new AVLNode<K, V>(nullptr, key, value);
        return true;
    }
    while (true) {
        if (pointer->key == key)
            return false;
        else if (pointer->key < key) {
            if (pointer->right != nullptr) {
                pointer = pointer->right;
            } else { // Insert in right
                pointer->right = new AVLNode<K, V>(pointer, key, value);
                updateNode(pointer);
                return true;
            }
        } else {
            if (pointer->left != nullptr) {
                pointer = pointer->left;
            } else { // Insert in left
                pointer->left = new AVLNode<K, V>(pointer, key, value);
                updateNode(pointer);
                return true;
            }
        }
    }
}

/**
 * Removes element with given key,
 * 
 * @param key
 * @return Was node removed?
 */
template <class K, class V>
bool AVLTree<K, V>::remove(const K &key) {
    auto *pointer = search(key);
    if (pointer == nullptr)
        return false;
    auto *parent = pointer->parent;
    // Case 1: No children
    if (pointer->left == nullptr && pointer->right == nullptr) {
        if (parent == nullptr) {
            // This was the only node in tree
            root = nullptr;
        } else {
            if (parent->left == pointer)
                parent->left = nullptr;
            else
                parent->right = nullptr;
        }
        delete pointer;
        updateNode(parent);
    } else if (pointer->left == nullptr && pointer->right != nullptr) {
        // Case 2a: One child on right
        if (parent == nullptr) {
            // Delete root, his right child is now the only node in tree
            root = pointer->right;
            root->parent = nullptr;
            pointer->right = nullptr;
        } else {
            if (parent->left == pointer)
                parent->left = pointer->right;
            else
                parent->right = pointer->right;
            pointer->right->parent = parent;
            pointer->right = nullptr;
        }
        delete pointer;
        updateNode(parent);
    } else if (pointer->left != nullptr && pointer->right == nullptr) {
        // Case 2b: One child on left
        if (parent == nullptr) {
            // Delete root, his left child is now the only node in tree
            root = pointer->left;
            root->parent = nullptr;
            pointer->left = nullptr;
        } else {
            if (parent->left == pointer)
                parent->left = pointer->left;
            else
                parent->right = pointer->left;
            pointer->left->parent = parent;
            pointer->left = nullptr;
        }
        delete pointer;
        updateNode(parent);
    } else {
        // Case 3: Two children
        auto *nextNode = getNextNode(pointer);
        K nextNodeKey = nextNode->key;
        V nextNodeValue = nextNode->value;
        // nextNode is guaranteed to have null as left child
        remove(nextNodeKey);
        pointer->key = nextNodeKey;
        pointer->value = nextNodeValue;
        updateNode(pointer);
    }
    return true;
}

/**
 * Example: GCD of set
 * First line of input: N
 * N next lines: c n
 *  * n is number to add or remove.
 *  * c is char '+' or '-' (add or remove n). It is assumed that when we remove
 *      n, it is already in set.
 * Output: After each of N lines, output GCD of all numbers in set.
 *         GCD of empty set is 1.
 */

class GCD {
    private:
        int init_n;
        int gcd(int a, int b) {
            if (b == 0) 
                return a;
            return gcd(b, (a % b));
        }
        int id;
        
    public:
        int gcd_value;
        GCD(int n, int id): init_n(n), gcd_value(n), id(id) {}
        void update(const GCD *e1, const GCD *e2) {
            if (e1 == nullptr && e2 == nullptr)
                gcd_value = init_n;
            else if (e1 == nullptr && e2 != nullptr)
                gcd_value = gcd(e2->gcd_value, init_n);
            else if (e1 != nullptr && e2 == nullptr)
                gcd_value = gcd(e1->gcd_value, init_n);
            else
                gcd_value = gcd(gcd(e1->gcd_value, e2->gcd_value), init_n);
        }
};

#include <iostream>

using namespace std;

int main() {
    ios_base::sync_with_stdio(0);
    AVLTree<int, GCD> tree;
    int n, k;
    char c;
    cin >> n;
    while (n--) {
        cin >> c >> k;
        if (c == '+')
            tree.insert(k, GCD(k, n));
        else
            tree.remove(k);
        if (tree.empty())
            cout << "1\n";
        else
           cout << tree.getValueAtRoot().gcd_value << endl;
    }
}
