import { Transaction } from './../../dtos/transactions/transaction.dto';
import { environment } from './../../environments/environments';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, Observable, throwError } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  httpOptions = {
    headers: new HttpHeaders(
      { 'Content-Type': 'application/json' }
    )
  };

  baseUrl = environment.api.hostBackend;
  transactionControllerUrl = environment.api.transactionsControllerBackend;

  getAllTransactions(): Observable<Transaction[]> {
    return this.httpClient.get<Transaction[]>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getAllTransactions}`
    )
      .pipe(
        map((transactions: Transaction[]) => {
          transactions.forEach((transaction) => {

          })
          return transactions;
        }),
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getTransactionsInMonth(): Observable<Transaction[]> {
    return this.httpClient.get<Transaction[]>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getTransactionsInMonth}`
    )
    .pipe(
      map((monthTransactions: Transaction[]) => {
        monthTransactions.forEach((monthTransaction) => {

        })
        return monthTransactions;
      }),
      catchError((erroResponse) => {
        return throwError(erroResponse);
      })
    );
  }

  getTransaction(): Observable<Transaction> {
    return this.httpClient.get<Transaction>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getTransaction
      }/1?transactionType=IN_CASH`
    )
    .pipe(
      catchError((erroResponse) => {
        return throwError(erroResponse);
      })
    );
  }

}
