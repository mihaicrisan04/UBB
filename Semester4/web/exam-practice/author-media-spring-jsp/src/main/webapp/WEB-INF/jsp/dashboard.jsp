<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/functions" prefix="fn" %>
<html>
<head>
    <title>Store Dashboard</title>
    <style>
        body { font-family: sans-serif; padding: 20px; }
        .error { background: rgb(255, 9, 9)}
        .success { background: rgb(118, 255, 13);}
    </style>
</head>
<body>
    <div class="header">
        <h1>Welcome to the Store, ${user.username}!</h1>
        <a href="/logout" class="logout">Logout</a>
    </div>

    <c:if test="${not empty error}">
        <div class="error">${error}</div>
    </c:if>

    <c:if test="${orderConfirmed}">
        <div class="success">
            Order confirmed successfully! 
            <br>Original Price: $${originalPrice}
            <c:if test="${discountPercent > 0}">
                <br>Discount Applied: ${discountPercent}%
            </c:if>
            <br><strong>Final Price: $${finalPrice}</strong>
        </div>
    </c:if>

    <div class="container">
        <div class="section">
            <h2>Available Products</h2>
            <div class="products-grid">
                <c:forEach var="product" items="${products}">
                    <div class="product-card">
                        <h4>${product.name}</h4>
                        <div class="price">$${product.price}</div>
                        <form action="/add-product" method="post" style="margin: 0;">
                            <input type="hidden" name="productId" value="${product.id}">
                            <button type="submit">Add to Order</button>
                        </form>
                    </div>
                </c:forEach>
            </div>
        </div>

        <div class="section">
            <h2>Current Order</h2>
            <c:choose>
                <c:when test="${fn:length(currentOrder) > 0}">
                    <c:forEach var="product" items="${currentOrder}" varStatus="status">
                        <div class="order-item">
                            <span>${product.name} - $${product.price}</span>
                            <form action="/remove-product" method="post" style="margin: 0;">
                                <input type="hidden" name="index" value="${status.index}">
                                <button type="submit">Remove</button>
                            </form>
                        </div>
                    </c:forEach>
                    
                    <div class="total">
                        Total: $${totalPrice}
                    </div>

                    <c:if test="${fn:length(currentOrder) >= 3}">
                        <div class="discount-info">
                            3+ products discount: 10% off
                        </div>
                    </c:if>

                    <c:if test="${not empty warning}">
                        <div class="warning" style="margin: 10px 0;">
                            ${warning}
                        </div>
                    </c:if>

                    <form action="/confirm-order" method="post" style="margin-top: 20px;">
                        <button type="submit" class="btn btn-success">Confirm Order</button>
                    </form>
                </c:when>
                <c:otherwise>
                    <p>Your order is empty. Add some products to get started</p>
                </c:otherwise>
            </c:choose>
        </div>
    </div>

    <div class="section" style="margin-top: 20px;">
        <h2>Your Recent Orders</h2>
        <c:choose>
            <c:when test="${fn:length(recentOrders) > 0}">
                <table style="width: 100%; border-collapse: collapse;">
                    <thead>
                        <tr>
                            <th>Order ID</th>
                            <th>Total Price</th>
                        </tr>
                    </thead>
                    <tbody>
                        <c:forEach var="order" items="${recentOrders}">
                            <tr>
                                <td style="border: 1px solid #000000;">#${order.id}</td>
                                <td style="border: 1px solid #000000;">$${order.totalPrice}</td>
                            </tr>
                        </c:forEach>
                    </tbody>
                </table>
            </c:when>
            <c:otherwise>
                <p>No previous orders found.</p>
            </c:otherwise>
        </c:choose>
    </div>

</body>
</html>