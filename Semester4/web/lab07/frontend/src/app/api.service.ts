import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

interface HelloResponse {
  message: string;
}

@Injectable({
  providedIn: 'root'
})
export class ApiService {
  private apiUrl = 'http://localhost:8000/api'; // Adjust if your backend runs on a different port/host

  constructor(private http: HttpClient) { }

  getHelloMessage(): Observable<HelloResponse> {
    return this.http.get<HelloResponse>(`${this.apiUrl}/hello`);
  }
} 