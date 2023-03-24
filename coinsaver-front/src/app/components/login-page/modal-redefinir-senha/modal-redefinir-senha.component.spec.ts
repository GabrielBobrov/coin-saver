import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ModalRedefinirSenhaComponent } from './modal-redefinir-senha.component';

describe('ModalRedefinirSenhaComponent', () => {
  let component: ModalRedefinirSenhaComponent;
  let fixture: ComponentFixture<ModalRedefinirSenhaComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ModalRedefinirSenhaComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ModalRedefinirSenhaComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
