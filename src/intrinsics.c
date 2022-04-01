#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define INITIAL_CAPACITY 16

typedef struct Deque {
    double *data;
    size_t capacity;
    size_t size;
    size_t offset;
} Deque;

Deque *__newDeque() {
    Deque *deque = malloc(sizeof(Deque));
    if (deque == NULL) {
        puts("An error ocurred trying to allocate deque");
        exit(1);
    }

    deque->data = malloc(sizeof(double) * INITIAL_CAPACITY);
    if (deque->data == NULL) {
        puts("An error ocurred trying to allocate deque's backing array");
        free(deque);
        exit(1);
    }

    deque->capacity = INITIAL_CAPACITY;
    deque->size = 0;
    deque->offset = 0;

    return deque;
}

void __delDeque(Deque *deque) {
    if (deque != NULL) {
        free(deque->data);
        free(deque);
    }
}

// Function to interface with savo to automaticall convert size to double
double __sizeOfDeque(Deque *deque) {
    return deque->size;
}

void __resizeDeque(Deque *deque, size_t newCapacity) {
    if (deque == NULL) return;

    double *newData = malloc(sizeof(double) * newCapacity);
    if (newData == NULL) {
        puts("An error ocurred trying to resize deque");
        __delDeque(deque);
        exit(1);
    }

    double *head = deque->data + deque->offset;
    for (size_t i = 0; i < deque->size; i++) {
        newData[i] = head[i];
    }

    free(deque->data);

    deque->capacity = newCapacity;
    deque->data = newData;
    deque->offset = 0;
}

// Halves the capacity of the deque if the size is half the capacity
void __shrinkDequeIfHalf(Deque *deque) {
    if (deque->capacity <= INITIAL_CAPACITY) return;

    size_t newCapacity = fmax(deque->capacity / 2, INITIAL_CAPACITY);
    if (deque->size >= newCapacity) return;

    __resizeDeque(deque, newCapacity);
}

// Returns the most recent element added
double __peekDeque(Deque *deque) {
    if (deque->size == 0) {
        return NAN;
    }

    return deque->data[deque->offset + deque->size - 1];
}

// Returns the earliest element added
double __peekHeadDeque(Deque *deque) {
    if (deque->size == 0) {
        return NAN;
    }

    return deque->data[deque->offset];
}

void __pushDeque(Deque *deque, double value) {
    if (deque->offset + deque->size + 1 >= deque->capacity) {
        __resizeDeque(deque, deque->size * 3 / 2);
    }

    deque->data[deque->offset + deque->size] = value;
    deque->size++;
}

double __popDeque(Deque *deque) {
    if (deque->size == 0) {
        return NAN;
    }

    // Need to save value first in case the deque gets resized
    double value = __peekDeque(deque);

    deque->size--;
    __shrinkDequeIfHalf(deque);

    return value;
}

double __popHeadDeque(Deque *deque) {
    if (deque->size == 0) {
        return NAN;
    }

    // Need to save value first in case the deque gets resized
    double value = __peekHeadDeque(deque);

    deque->size--;
    deque->offset++;
    __shrinkDequeIfHalf(deque);

    return value;
}

void __putfc(double n) {
    char ch = (char)n;
    putchar(ch);
}

double __getfc() {
    char ch = getchar();
    return (double)ch;
}

void __dumpf(double n) {
    printf("%.16g\n", n);
}