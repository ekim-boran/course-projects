// Order book for high-frequency trading

typedef struct orderBook *OrderBook;

// Make a new empty order book.
OrderBook orderBookCreate(void);

// Destroy an order book,
// freeing all space
// and discarding all pending orders.
void orderBookDestroy(OrderBook);

// Enter a new order in the order book.
//
// If price > 0, it represents a buy order.
// Return value will be price p2 of sell order
// maximizing price + p2 > 0, or 0 if there
// is no such sell order.
//
// If price < 0, it represents a sell order.
// Return value will be price p2 of buy order
// maximizing price + p2 > 0, or 0 if there
// is no such buy order.
//
// In either of the above cases, if 0 is returned,
// then price is entered into the order book
// as a pending order available for future matches.
// If a nonzero value is returned, the corresponding
// pending order is removed.
//
// If price == 0, no order is entered and 
// return value is 0.
int orderBookInsert(OrderBook, int price);
