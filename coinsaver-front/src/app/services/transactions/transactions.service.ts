import { TransactionResponseDto } from '../../dtos/transactions/response/transaction.response.dto';
import { environment } from './../../environments/environments';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, Observable, throwError } from 'rxjs';
import { PayTransactionRequestDto } from 'src/app/dtos/transactions/request/pay-transaction.request.dto';
import { TransactionRequestDto } from 'src/app/dtos/transactions/request/transaction.request.dto';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { MonthlyChartDivisionResponseDto } from 'src/app/dtos/transactions/response/monthly-chart-division.response.dto';
import { MonthlyChartResponseDto } from 'src/app/dtos/transactions/response/monthly-chart.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { PerformanceResponseDto } from 'src/app/dtos/transactions/response/performance.response.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  baseUrl = environment.api.hostBackend;
  transactionControllerUrl = environment.api.transactionsControllerBackend;

  token?: string;

  recebeToken(token: any) {
    this.token = JSON.stringify(token.token).replaceAll('"', '');
  }

  getAllTransactions(): Observable<TransactionResponseDto[]> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    console.log(httpOptions)

    return this.httpClient.get<TransactionResponseDto[]>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getAllTransactions
      }`), httpOptions
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

  getTransaction(transactionId: number, transactionType: TransactionTypeEnum): Observable<TransactionResponseDto> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<TransactionResponseDto>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getTransaction
      }/${transactionId
      }?transactionType=${transactionType}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getTransactionByCategoryType(categoryType: TransactionCategoryTypeEnum, date: string): Observable<TransactionResponseDto[]> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<TransactionResponseDto[]>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getTransactionByCategoryType
      }/${categoryType
      }?date=${date}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  createTransaction(transactionRequestDto: TransactionRequestDto) {
    const url = this.baseUrl + this.transactionControllerUrl + environment.api.transactionsBackendEndpoints.createTransaction;
    const body = (transactionRequestDto);
    const headers = { 'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}` };

    return this.httpClient.post(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }

  getTransactionsInMonth(date: string): Observable<MonthlyResponseDto> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<MonthlyResponseDto>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getTransactionsInMonth
      }?date=${date}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  updateTransaction(updateTransactionRequestDto: UpdateTransactionRequestDto) {
    const url = this.baseUrl + this.transactionControllerUrl + environment.api.transactionsBackendEndpoints.updateTransaction;
    const body = updateTransactionRequestDto;
    const headers = { 'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}` };

    return this.httpClient.put(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }

  updateTransactionPatch(payTransactionRequestDto: PayTransactionRequestDto) {
    const url = this.baseUrl + this.transactionControllerUrl + environment.api.transactionsBackendEndpoints.updateTransactionPatch;
    const body = JSON.stringify(payTransactionRequestDto);
    const headers = { 'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}` };

    console.log(url, body, { headers })

    return this.httpClient.patch(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }

  deleteByTransactionId(transactionId: number) {
    const url = this.baseUrl + this.transactionControllerUrl + environment.api.transactionsBackendEndpoints.deleteByTransactionId;
    const headers = { 'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}` };

    return this.httpClient.delete(`${url}/${transactionId}`, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }

  getTransactionsAmountByCategory(date: string): Observable<MonthlyChartResponseDto> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<MonthlyChartResponseDto>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getTransactionsAmountByCategory
      }?date=${date}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getTransactionsAmountByDivision(date: string, categoryType: string): Observable<MonthlyChartDivisionResponseDto> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<MonthlyChartDivisionResponseDto>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getTransactionsAmountByCategory
      }/${categoryType}/divisions?date=${date}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getPerformance(date: string): Observable<PerformanceResponseDto> {
    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<PerformanceResponseDto>(
      (`${this.baseUrl +
      this.transactionControllerUrl +
      environment.api.transactionsBackendEndpoints.getPerformance
      }?date=${date}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

}
