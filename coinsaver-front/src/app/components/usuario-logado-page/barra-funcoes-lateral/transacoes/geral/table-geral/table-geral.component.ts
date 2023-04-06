import { Component, OnInit } from '@angular/core';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';

@Component({
  selector: 'app-table-geral',
  templateUrl: './table-geral.component.html',
  styleUrls: ['./table-geral.component.css']
})
export class TableGeralComponent implements OnInit {

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];

  constructor(
    private transactionsService: TransactionsService,
    ) {}

  ngOnInit() {
      this.getAllTransactions();
  }

  getAllTransactions() {
    this.transactionsService.getAllTransactions()
      .subscribe((res) => {

        console.log('all', res)
      });
  }

}
