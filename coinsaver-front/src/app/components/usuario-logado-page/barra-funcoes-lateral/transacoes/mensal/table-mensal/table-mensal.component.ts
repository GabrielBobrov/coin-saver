import { Component, OnInit } from '@angular/core';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';

@Component({
  selector: 'app-table-mensal',
  templateUrl: './table-mensal.component.html',
  styleUrls: ['./table-mensal.component.css']
})
export class TableMensalComponent implements OnInit {

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];

  date: string = '';
  dataUtils = new DataUtils();

  constructor(
    private transactionsService: TransactionsService,
    ) {}

  ngOnInit() {
      this.getTransactionsInMonth();
  }

  getTransactionsInMonth() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');

    this.transactionsService.getTransactionsInMonth(this.date)
      .subscribe((res) => {
        this.monthlyResponseDto = res;

        this.monthlyResponseDto.transactions?.forEach((transaction) => {
        })

        this.monthlyTransactionsResponseDtoList = this.monthlyResponseDto.transactions;

        console.log('month', this.monthlyResponseDto)
      });
  }

}
