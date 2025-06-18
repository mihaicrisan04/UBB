package com.mocktest.app.controller;

import com.mocktest.app.model.*;
import com.mocktest.app.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.support.SessionStatus;
import java.util.*;
import java.util.stream.Collectors;

@Controller
@SessionAttributes("user") // Store "author" object in the session
public class MainController {

    @Autowired private AuthorRepository authorRepo;
    @Autowired private DocumentRepository docRepo;
    @Autowired private MovieRepository movieRepo;

    @Autowired private MovieRepository userRepo;
    @Autowired private MovieRepository productRepo;
    @Autowired private MovieRepository orderRepo;


    // Show login page
    @GetMapping("/")
    public String index(Model model) {
        try {
            // Test database connection
            long authorCount = authorRepo.count();
            model.addAttribute("authorCount", authorCount);
            return "index"; // Renders /WEB-INF/jsp/index.jsp
        } catch (Exception e) {
            model.addAttribute("error", "Database connection error: " + e.getMessage());
            return "index";
        }
    }

    // Handle login
    @PostMapping("/login")
    public String login(@RequestParam String name, Model model) {
        try {
            Optional<User> userOpt = userRepo.findByName(name);
            if (userOpt.isPresent()) {
                model.addAttribute("user", userOpt.get()); // Add to session
                return "redirect:/dashboard"; 
            }
            model.addAttribute("error", "User not found.");

            Optional<Author> authorOpt = authorRepo.findByName(name);
            if (authorOpt.isPresent()) {
                model.addAttribute("author", authorOpt.get()); // Add to session
                return "redirect:/dashboard";
            }
            model.addAttribute("error", "Author not found.");
            return "index";
        } catch (Exception e) {
            model.addAttribute("error", "Login error: " + e.getMessage());
            return "index";
        }
    }

    // Show dashboard
    @GetMapping("/dashboard")
    public String dashboard(@ModelAttribute(value="author", binding=false) Author author, Model model) {
        try {
            if (author == null) {
                return "redirect:/";
            }
            
            // Fetch documents and movies
            List<Integer> docIds = parseIds(author.getDocumentList());
            List<Integer> movieIds = parseIds(author.getMovieList());
            model.addAttribute("documents", docRepo.findAllById(docIds));
            model.addAttribute("movies", movieRepo.findAllById(movieIds));

            // Find document with most authors
            findDocWithMostAuthors(model);

            return "dashboard";
        } catch (Exception e) {
            model.addAttribute("error", "Dashboard error: " + e.getMessage());
            return "redirect:/";
        }
    }

    // Handle adding a document
    @PostMapping("/documents/add")
    public String addDocument(@ModelAttribute("author") Author author, @RequestParam String docName, @RequestParam String docContents) {
        Document newDoc = new Document();
        newDoc.setName(docName);
        newDoc.setContents(docContents);
        docRepo.save(newDoc); // Save returns the persisted entity with ID

        String currentList = author.getDocumentList();
        String newList = (currentList == null || currentList.isEmpty())
            ? String.valueOf(newDoc.getId())
            : currentList + "," + newDoc.getId();
        author.setDocumentList(newList);
        authorRepo.save(author);

        return "redirect:/dashboard";
    }

    // Handle deleting a movie
    @GetMapping("/movies/delete/{id}")
    public String deleteMovie(@ModelAttribute("author") Author author, @PathVariable int id) {
        List<String> movieIds = new ArrayList<>(Arrays.asList(author.getMovieList().split(",")));
        movieIds.remove(String.valueOf(id));
        author.setMovieList(String.join(",", movieIds));
        authorRepo.save(author);
        return "redirect:/dashboard";
    }

    // Handle logout
    @GetMapping("/logout")
    public String logout(SessionStatus status) {
        status.setComplete(); // Clear the session
        return "redirect:/";
    }

    // Helper methods
    private List<Integer> parseIds(String idList) {
        if (idList == null || idList.trim().isEmpty()) {
            return Collections.emptyList();
        }
        return Arrays.stream(idList.split(","))
                     .map(Integer::parseInt)
                     .collect(Collectors.toList());
    }

    private void findDocWithMostAuthors(Model model) {
        Map<Integer, Long> docAuthorCounts = authorRepo.findAll().stream()
            .map(Author::getDocumentList)
            .filter(Objects::nonNull)
            .flatMap(list -> Arrays.stream(list.split(",")))
            .filter(id -> !id.isEmpty())
            .map(Integer::parseInt)
            .collect(Collectors.groupingBy(id -> id, Collectors.counting()));

        Optional<Map.Entry<Integer, Long>> maxEntry = docAuthorCounts.entrySet()
            .stream()
            .max(Map.Entry.comparingByValue());

        if (maxEntry.isPresent()) {
            docRepo.findById(maxEntry.get().getKey())
                   .ifPresent(doc -> model.addAttribute("docWithMostAuthors", doc));
        }
    }
}