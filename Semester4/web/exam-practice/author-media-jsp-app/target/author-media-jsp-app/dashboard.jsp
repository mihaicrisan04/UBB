<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/functions" prefix="fn" %>
<html>
<head>
    <title>Dashboard</title>
    <style>
        body { font-family: sans-serif; padding: 20px; }
        h1, h2 { border-bottom: 1px solid #ccc; padding-bottom: 5px; }
        table { border-collapse: collapse; width: 100%; margin-top: 10px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        form { margin-top: 20px; padding: 15px; border: 1px solid #ccc; }
        .logout { float: right; }
        .card { border: 1px solid #007bff; padding: 15px; margin-top: 20px; border-radius: 5px; }
    </style>
</head>
<body>
    <a href="app?action=logout" class="logout">Logout</a>
    <h1>Welcome, ${sessionScope.author.name}</h1>

    <!-- Add New Document -->
    <h2>Add New Document</h2>
    <form action="app" method="post">
        <input type="hidden" name="action" value="addDocument">
        <div>
            <label for="docName">Name:</label>
            <input type="text" id="docName" name="docName" required>
        </div>
        <div style="margin-top: 10px;">
            <label for="docContents">Contents:</label>
            <textarea id="docContents" name="docContents" rows="4" cols="50" required></textarea>
        </div>
        <button type="submit" style="margin-top: 10px;">Add Document</button>
    </form>

    <!-- Interleaved List -->
    <h2>Your Authored Works</h2>
    <table>
        <thead>
            <tr>
                <th>Type</th>
                <th>Title / Name</th>
                <th>Details</th>
                <th>Action</th>
            </tr>
        </thead>
        <tbody>
            <c:set var="maxSize" value="${fn:length(documents) > fn:length(movies) ? fn:length(documents) : fn:length(movies)}" />
            <c:forEach var="i" begin="0" end="${maxSize - 1}">
                <c:if test="${i < fn:length(documents)}">
                    <tr>
                        <td>Document</td>
                        <td>${documents[i].name}</td>
                        <td>${documents[i].contents}</td>
                        <td>-</td>
                    </tr>
                </c:if>
                <c:if test="${i < fn:length(movies)}">
                    <tr>
                        <td>Movie</td>
                        <td>${movies[i].title}</td>
                        <td>Duration: ${movies[i].duration} mins</td>
                        <td>
                            <a href="app?action=deleteMovie&id=${movies[i].id}">Delete</a>
                        </td>
                    </tr>
                </c:if>
            </c:forEach>
        </tbody>
    </table>

    <!-- Document with Most Authors -->
    <h2>Document with the Most Authors</h2>
    <div class="card">
        <c:if test="${not empty docWithMostAuthors}">
            <strong>Name:</strong> ${docWithMostAuthors.name}<br>
            <strong>Contents:</strong> ${docWithMostAuthors.contents}
        </c:if>
        <c:if test="${empty docWithMostAuthors}">
            <p>No documents found.</p>
        </c:if>
    </div>

</body>
</html>