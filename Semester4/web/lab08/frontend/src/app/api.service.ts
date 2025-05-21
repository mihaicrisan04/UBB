import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class ApiService {
  private apiUrl = 'http://localhost:5213/api'; // Adjust if your backend runs on a different port/host

  constructor(private http: HttpClient) { }
} 