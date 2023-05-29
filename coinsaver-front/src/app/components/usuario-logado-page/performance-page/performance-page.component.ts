import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { ModalTrocarSenhaComponent } from '../modal-trocar-senha/modal-trocar-senha.component';
import { DialogService } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-performance-page',
  templateUrl: './performance-page.component.html',
  styleUrls: ['./performance-page.component.css']
})
export class PerformancePageComponent implements OnInit {

  date: string = '';
  dataUtils = new DataUtils();

  dataPerformance: any;
  optionsPerformance: any;

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    private dialogService: DialogService,
  ) { }

  ngOnInit(): void {
    this.formataDataInicialTabelaMensal();
  }

  formataDataInicialTabelaMensal() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');
    this.getPerformance(this.date);
  }

  private getPerformance(date: any) {
    this.transactionsService.getTransactionsAmountByCategory(date)
    .subscribe((res) => {
      console.log(res)
    });
  }

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  abrirPerformancePage() {
    this.router.navigateByUrl('performance-page', {
      state: {
        data: {},
      },
    });
  }

  modalTrocarSenha() {
    this.dialogService.open(ModalTrocarSenhaComponent, {
      data: {},
      showHeader: false
    });
  }

}
