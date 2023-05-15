import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, throwError } from 'rxjs';
import { DivisionResponseDto } from 'src/app/dtos/transactions/response/division.response.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { environment } from 'src/app/environments/environments';

@Injectable({
  providedIn: 'root'
})
export class DivisionsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  baseUrl = environment.api.hostBackend;
  divisionControllerUrl = environment.api.divisionsControllerBackend;

  token?: string;

  recebeToken(token: any) {
    this.token = JSON.stringify(token.token).replaceAll('"', '');
  }

  getDivisionByCategoryType(categoryType: TransactionCategoryTypeEnum): Observable<DivisionResponseDto[]> {

    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    console.log(httpOptions)

    return this.httpClient.get<DivisionResponseDto[]>(
      (`${this.baseUrl +
      this.divisionControllerUrl +
      environment.api.divisionsBackendEndpoints.getDivisionByCategoryType
      }/${categoryType}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getDivisionsById(divisionId: number): Observable<DivisionResponseDto> {

    const headerDict  = {'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}`};
    const httpOptions = {
      headers: new HttpHeaders(headerDict)
    };

    return this.httpClient.get<DivisionResponseDto>(
      (`${this.baseUrl +
      this.divisionControllerUrl +
      environment.api.divisionsBackendEndpoints.getDivisionById
      }/${divisionId}`), httpOptions
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }
}
