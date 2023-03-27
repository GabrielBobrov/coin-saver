import { Transaction } from './../../dtos/transactions/transaction.dto';
import { environment } from './../../environments/environments';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  httpOptions = {
    headers : new HttpHeaders(
      {'Content-Type':'application/json'}
    )
  };

  baseUrl = environment.api.hostBackend;
  transactionControllerUrl = environment.api.transactionsControllerBackend;

  getAllTransactions(): Observable<Transaction[]> {
    return this.httpClient.get<Transaction[]>(
      `${
        this.baseUrl +
        this.transactionControllerUrl +
        environment.api.backendEndpoints.getTransactions}`
    )
    .pipe(

      catchError((erroResponse) => {
        return throwError(erroResponse);
      })

    );
  }

  getMonthTransactions(): Observable<Transaction[]> {
    return this.httpClient.get<Transaction[]>(
      `${
        this.baseUrl +
        this.transactionControllerUrl +
        environment.api.backendEndpoints.getTransactionsMonth}`
    )
    .pipe(

      catchError((erroResponse) => {
        return throwError(erroResponse);
      })

    );
  }
}
