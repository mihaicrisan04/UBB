package com.mocktest.app.controller;

import com.mocktest.app.model.*;
import com.mocktest.app.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.support.SessionStatus;

import jakarta.servlet.http.HttpSession;
import java.util.*;
import java.util.stream.Collectors;

@Controller
@SessionAttributes("user")
public class MainController {

    @Autowired private UserRepository userRepo;
    @Autowired private ProductRepository productRepo;
    @Autowired private OrderRepository orderRepo;
    @Autowired private OrderItemRepository orderItemsRepo;

    @GetMapping("/")
    public String index(Model model) {
        try {
            long userCount = userRepo.count();
            model.addAttribute("userCount", userCount);
            return "index";
        } catch (Exception e) {
            model.addAttribute("error", "Database connection error: " + e.getMessage());
            return "index";
        }
    }

    @PostMapping("/login")
    public String login(@RequestParam String name, Model model, HttpSession session) {
        try {
            Optional<User> userOpt = userRepo.findByUsername(name);
            if (userOpt.isPresent()) {
                model.addAttribute("user", userOpt.get());
                session.setAttribute("currentOrder", new ArrayList<Product>());
                return "redirect:/dashboard";
            }
            model.addAttribute("error", "User not found.");
            return "index";
        } catch (Exception e) {
            model.addAttribute("error", "Login error: " + e.getMessage());
            return "index";
        }
    }

    @GetMapping("/dashboard")
    public String dashboard(@ModelAttribute(value="user", binding=false) User user, Model model, HttpSession session) {
        try {
            if (user == null) {
                return "redirect:/";
            }
            
            List<Product> allProducts = productRepo.findAll();
            model.addAttribute("products", allProducts);
            
            @SuppressWarnings("unchecked")
            List<Product> currentOrder = (List<Product>) session.getAttribute("currentOrder");
            if (currentOrder == null) {
                currentOrder = new ArrayList<>();
                session.setAttribute("currentOrder", currentOrder);
            }
            model.addAttribute("currentOrder", currentOrder);
            
            int totalPrice = currentOrder.stream().mapToInt(Product::getPrice).sum();
            model.addAttribute("totalPrice", totalPrice);
            
            Set<String> categoriesInCurrentOrder = currentOrder.stream()
                    .map(p -> extractCategory(p.getName()))
                    .collect(Collectors.toSet());
            
            String diversificationWarning = null;
            for (String category : categoriesInCurrentOrder) {
                String warning = checkDiversification(user.getId(), category);
                if (warning != null) {
                    diversificationWarning = warning;
                    break;
                }
            }
            if (diversificationWarning != null) {
                model.addAttribute("warning", diversificationWarning);
            }
            
            // Handle order confirmation from session
            Boolean orderConfirmed = (Boolean) session.getAttribute("orderConfirmed");
            if (orderConfirmed != null && orderConfirmed) {
                model.addAttribute("orderConfirmed", true);
                model.addAttribute("finalPrice", session.getAttribute("finalPrice"));
                model.addAttribute("originalPrice", session.getAttribute("originalPrice"));
                model.addAttribute("discountPercent", session.getAttribute("discountPercent"));
                // Clear the confirmation from session
                session.removeAttribute("orderConfirmed");
                session.removeAttribute("finalPrice");
                session.removeAttribute("originalPrice");
                session.removeAttribute("discountPercent");
            }
            
            List<Order> userOrders = orderRepo.findByUserIdOrderByIdDesc(user.getId())
                    .stream().limit(3).collect(Collectors.toList());
            model.addAttribute("recentOrders", userOrders);
            
            return "dashboard";
        } catch (Exception e) {
            model.addAttribute("error", "Dashboard error: " + e.getMessage());
            return "redirect:/";
        }
    }

    @PostMapping("/add-product")
    public String addProduct(@RequestParam int productId, @ModelAttribute("user") User user, HttpSession session) {
        try {
            Optional<Product> productOpt = productRepo.findById(productId);
            if (productOpt.isPresent()) {
                Product product = productOpt.get();
                
                @SuppressWarnings("unchecked")
                List<Product> currentOrder = (List<Product>) session.getAttribute("currentOrder");
                if (currentOrder == null) {
                    currentOrder = new ArrayList<>();
                }
                
                currentOrder.add(product);
                session.setAttribute("currentOrder", currentOrder);
            }
            return "redirect:/dashboard";
        } catch (Exception e) {
            return "redirect:/dashboard";
        }
    }

    @PostMapping("/remove-product")
    public String removeProduct(@RequestParam int index, HttpSession session) {
        try {
            @SuppressWarnings("unchecked")
            List<Product> currentOrder = (List<Product>) session.getAttribute("currentOrder");
            if (currentOrder != null && index >= 0 && index < currentOrder.size()) {
                currentOrder.remove(index);
                session.setAttribute("currentOrder", currentOrder);
            }
            return "redirect:/dashboard";
        } catch (Exception e) {
            return "redirect:/dashboard";
        }
    }

    @PostMapping("/confirm-order")
    public String confirmOrder(@ModelAttribute("user") User user,
                             HttpSession session) {
        try {
            @SuppressWarnings("unchecked")
            List<Product> currentOrder = (List<Product>) session.getAttribute("currentOrder");
            
            if (currentOrder == null || currentOrder.isEmpty()) {
                return "redirect:/dashboard";
            }
            
            int totalPrice = currentOrder.stream().mapToInt(Product::getPrice).sum();
            double discount = calculateDiscount(currentOrder);
            int finalPrice = (int) (totalPrice * (1 - discount));
            
            Order order = new Order();
            order.setUserId(user.getId());
            order.setTotalPrice(finalPrice);
            Order savedOrder = orderRepo.save(order);

            for (Product product: currentOrder) {
                OrderItem orderItem = new OrderItem();
                orderItem.setOrderId(order.getId());
                orderItem.setProductId(product.getId());
                orderItemsRepo.save(orderItem);
            }
            
            session.setAttribute("currentOrder", new ArrayList<Product>());
            session.setAttribute("orderConfirmed", true);
            session.setAttribute("finalPrice", finalPrice);
            session.setAttribute("originalPrice", totalPrice);
            session.setAttribute("discountPercent", (int)(discount * 100));
            
            return "redirect:/dashboard";
        } catch (Exception e) {
            return "redirect:/dashboard";
        }
    }

    @GetMapping("/logout")
    public String logout(SessionStatus status, HttpSession session) {
        status.setComplete();
        session.invalidate();
        return "redirect:/";
    }

    private String extractCategory(String productName) {
        int dashIndex = productName.indexOf('-');
        if (dashIndex > 0) {
            return productName.substring(0, dashIndex);
        }
        return productName;
    }

    private double calculateDiscount(List<Product> products) {
        double discount = 0.0;
        
        if (products.size() >= 3) {
            discount += 0.10;
        }
        
        Map<String, Long> categoryCount = products.stream()
                .collect(Collectors.groupingBy(
                    p -> extractCategory(p.getName()),
                    Collectors.counting()
                ));
        
        boolean hasDuplicateCategory = categoryCount.values().stream()
                .anyMatch(count -> count >= 2);
        
        if (hasDuplicateCategory) {
            discount += 0.05;
        }
        
        return Math.min(discount, 0.15);
    }

    private String checkDiversification(int userId, String category) {
        try {
            List<Order> lastThreeOrders = orderRepo.findByUserIdOrderByIdDesc(userId)
                    .stream().limit(3).collect(Collectors.toList());
            
            if (lastThreeOrders.size() < 3) {
                return null;
            }
            
            int ordersWithCategory = 0;
            for (Order order : lastThreeOrders) {
                List<Product> orderProducts = getProductsForOrder(order.getId());
                boolean categoryInThisOrder = false;
                for (Product product : orderProducts) {
                    String productCategory = extractCategory(product.getName());
                    if (category.equals(productCategory)) {
                        categoryInThisOrder = true;
                        break;
                    }
                }
                if (categoryInThisOrder) {
                    ordersWithCategory++;
                }
            }
            
            if (ordersWithCategory == 3) {
                return "Warning: This category was present in all your last 3 orders. Consider diversifying your choices.";
            }
            
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    private List<Product> getProductsForOrder(int orderId) {
        List<OrderItem> orderItems = orderItemsRepo.findByOrderId(orderId);
        List<Product> products = new ArrayList<>();

        for (OrderItem orderItem: orderItems) {
            Optional<Product> product = productRepo.findById(orderItem.getProductId());
            if (product.isPresent()) {
                products.add(product.get());
            }
        }

        return products;
    }
}