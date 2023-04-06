import { TransactionResponseDto } from '../../dtos/transactions/response/transaction.response.dto';
import { environment } from './../../environments/environments';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, Observable, throwError } from 'rxjs';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';

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

  getAllTransactions(): Observable<TransactionResponseDto[]> {
    return this.httpClient.get<TransactionResponseDto[]>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getAllTransactions
    }`
    )
      .pipe(
        map((transactions: TransactionResponseDto[]) => {
          transactions.forEach((transaction) => {

          })
          return transactions;
        }),
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getTransactionsInMonth(date: string): Observable<MonthlyResponseDto> {
    return this.httpClient.get<MonthlyResponseDto>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getTransactionsInMonth
    }?date=${date}`
    )
    .pipe(
      catchError((erroResponse) => {
        return throwError(erroResponse);
      })
    );
  }

  getTransaction(transactionId: number, transactionType: TransactionTypeEnum): Observable<TransactionResponseDto> {
    return this.httpClient.get<TransactionResponseDto>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getTransaction
    }/${transactionId
    }?transactionType=${transactionType}`
    )
    .pipe(
      catchError((erroResponse) => {
        return throwError(erroResponse);
      })
    );
  }

  getTransactionByCategoryType(transactionCategoryType: TransactionCategoryTypeEnum, date: string): Observable<TransactionResponseDto> {
    return this.httpClient.get<TransactionResponseDto>(
      `${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.backendEndpoints.getTransactionByCategoryType
    }/${transactionCategoryType
    }?date=${date}`
    )
    .pipe(
      catchError((erroResponse) => {
        return throwError(erroResponse);
      })
    );
  }

}
